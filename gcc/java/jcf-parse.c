/* Parser for Java(TM) .class files.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "java-except.h"
#include "input.h"
#include "javaop.h"
#include "java-tree.h"
#include "diagnostic-core.h"
#include "parse.h"
#include "ggc.h"
#include "debug.h"
#include "cgraph.h"
#include "bitmap.h"
#include "target.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

/* A CONSTANT_Utf8 element is converted to an IDENTIFIER_NODE at parse time. */
#define JPOOL_UTF(JCF, INDEX) CPOOL_UTF(&(JCF)->cpool, INDEX)
#define JPOOL_UTF_LENGTH(JCF, INDEX) IDENTIFIER_LENGTH (JPOOL_UTF (JCF, INDEX))
#define JPOOL_UTF_DATA(JCF, INDEX) \
  ((const unsigned char *) IDENTIFIER_POINTER (JPOOL_UTF (JCF, INDEX)))
#define HANDLE_CONSTANT_Utf8(JCF, INDEX, LENGTH) \
  do { \
    unsigned char save;  unsigned char *text; \
    JCF_FILL (JCF, (LENGTH)+1); /* Make sure we read 1 byte beyond string. */ \
    text = (JCF)->read_ptr; \
    save = text[LENGTH]; \
    text[LENGTH] = 0; \
    (JCF)->cpool.data[INDEX].t = get_identifier ((const char *) text); \
    text[LENGTH] = save; \
    JCF_SKIP (JCF, LENGTH); } while (0)

#include "jcf.h"

extern struct obstack temporary_obstack;

static GTY(()) tree parse_roots[2];

/* The FIELD_DECL for the current field.  */
#define current_field parse_roots[0]

/* The METHOD_DECL for the current method.  */
#define current_method parse_roots[1]

/* Line 0 in current file, if compiling from bytecode. */
static location_t file_start_location;

/* The Java archive that provides main_class;  the main input file. */
static GTY(()) struct JCF * main_jcf;

/* A list of all the class DECLs seen so far.  */
static GTY(()) vec<tree, va_gc> *all_class_list;

/* The number of source files passed to us by -fsource-filename and an
   array of pointers to each name.  Used by find_sourcefile().  */
static int num_files = 0;
static char **filenames;

static struct ZipFile *localToFile;

/* A map of byte offsets in the reflection data that are fields which
   need renumbering.  */
bitmap field_offsets;
bitmap_obstack bit_obstack;

/* Declarations of some functions used here.  */
static void handle_innerclass_attribute (int count, JCF *, int len);
static tree give_name_to_class (JCF *jcf, int index);
static char *compute_class_name (struct ZipDirectory *zdir);
static int classify_zip_file (struct ZipDirectory *zdir);
static void parse_zip_file_entries (void);
static void process_zip_dir (FILE *);
static void parse_class_file (void);
static void handle_deprecated (void);
static void set_source_filename (JCF *, int);
static void jcf_parse (struct JCF*);
static void load_inner_classes (tree);
static void handle_annotation (JCF *jcf, int level);
static void java_layout_seen_class_methods (void);

/* Handle "Deprecated" attribute.  */
static void
handle_deprecated (void)
{
  if (current_field != NULL_TREE)
    FIELD_DEPRECATED (current_field) = 1;
  else if (current_method != NULL_TREE)
    METHOD_DEPRECATED (current_method) = 1;
  else if (current_class != NULL_TREE)
    CLASS_DEPRECATED (TYPE_NAME (current_class)) = 1;
  else
    {
      /* Shouldn't happen.  */
      gcc_unreachable ();
    }
}



/* Reverse a string.  */
static char *
reverse (const char *s)
{
  if (s == NULL)
    return NULL;
  else
    {
      int len = strlen (s);
      char *d = XNEWVAR (char, len + 1);
      const char *sp;
      char *dp;
      
      d[len] = 0;
      for (dp = &d[0], sp = &s[len-1]; sp >= s; dp++, sp--)
	*dp = *sp;

      return d;
    }
}

/* Compare two strings for qsort().  */
static int
cmpstringp (const void *p1, const void *p2)
{
  /* The arguments to this function are "pointers to
     pointers to char", but strcmp() arguments are "pointers
     to char", hence the following cast plus dereference */

  return strcmp(*(const char *const*) p1, *(const char *const*) p2);
}

/* Create an array of strings, one for each source file that we've
   seen.  fsource_filename can either be the name of a single .java
   file or a file that contains a list of filenames separated by
   newlines.  */
void 
java_read_sourcefilenames (const char *fsource_filename)
{
  if (fsource_filename 
      && filenames == 0
      && strlen (fsource_filename) > strlen (".java")
      && filename_cmp ((fsource_filename
		       + strlen (fsource_filename)
		       - strlen (".java")),
		 ".java") != 0)
    {
/*       fsource_filename isn't a .java file but a list of filenames
       separated by newlines */
      FILE *finput = fopen (fsource_filename, "r");
      int len = 0;
      int longest_line = 0;

      gcc_assert (finput);

      /* Find out how many files there are, and how long the filenames are.  */
      while (! feof (finput))
	{
	  int ch = getc (finput);
	  if (ch == '\n')
	    {
	      num_files++;
	      if (len > longest_line)
		longest_line = len;
	      len = 0;
	      continue;
	    }
	  if (ch == EOF)
	    break;
	  len++;
	}

      rewind (finput);

      /* Read the filenames.  Put a pointer to each filename into the
	 array FILENAMES.  */
      {
	char *linebuf = (char *) alloca (longest_line + 1);
	int i = 0;
	int charpos;

	filenames = XNEWVEC (char *, num_files);

	charpos = 0;
	for (;;)
	  {
	    int ch = getc (finput);
	    if (ch == EOF)
	      break;
	    if (ch == '\n')
	      {
		linebuf[charpos] = 0;
		gcc_assert (i < num_files);		
		/* ???  Perhaps we should use lrealpath() here.  Doing
		   so would tidy up things like /../ but the rest of
		   gcc seems to assume relative pathnames, not
		   absolute pathnames.  */
/* 		realname = lrealpath (linebuf); */
		filenames[i++] = reverse (linebuf);
		charpos = 0;
		continue;
	      }
	    gcc_assert (charpos < longest_line);
	    linebuf[charpos++] = ch;
	  }

	if (num_files > 1)
	  qsort (filenames, num_files, sizeof (char *), cmpstringp);
      }
      fclose (finput);
    }
  else
    {
      filenames = XNEWVEC (char *, 1);      
      filenames[0] = reverse (fsource_filename);
      num_files = 1;
    }
}

/* Given a relative pathname such as foo/bar.java, attempt to find a
   longer pathname with the same suffix.  

   This is a best guess heuristic; with some weird class hierarchies we
   may fail to pick the correct source file.  For example, if we have
   the filenames foo/bar.java and also foo/foo/bar.java, we do not
   have enough information to know which one is the right match for
   foo/bar.java.  */

static const char *
find_sourcefile (const char *name)
{
  int i = 0, j = num_files-1;
  char *found = NULL;
  
  if (filenames)
    {
      char *revname = reverse (name);

      do
	{
	  int k = (i+j) / 2;
	  int cmp = strncmp (revname, filenames[k], strlen (revname));
	  if (cmp == 0)
	    {
	      /*  OK, so we found one.  But is it a unique match?  */
	      if ((k > i
		   && strncmp (revname, filenames[k-1], strlen (revname)) == 0)
		  || (k < j
		      && (strncmp (revname, filenames[k+1], strlen (revname)) 
			  == 0)))
		;
	      else
		found = filenames[k];
	      break;
	    }
	  if (cmp > 0)
	    i = k+1;
	  else
	    j = k-1;
	}
      while (i <= j);

      free (revname);
    }

  if (found && strlen (found) > strlen (name))
    return reverse (found);
  else
    return name;
}



/* Handle "SourceFile" attribute. */

static void
set_source_filename (JCF *jcf, int index)
{
  tree sfname_id = get_name_constant (jcf, index);
  const char *sfname = IDENTIFIER_POINTER (sfname_id);
  const char *old_filename = input_filename;
  int new_len = IDENTIFIER_LENGTH (sfname_id);
  if (old_filename != NULL)
    {
      int old_len = strlen (old_filename);
      /* Use the current input_filename (derived from the class name)
	 if it has a directory prefix, but otherwise matches sfname. */
      if (old_len > new_len
	  && filename_cmp (sfname, old_filename + old_len - new_len) == 0
	  && (old_filename[old_len - new_len - 1] == '/'
	      || old_filename[old_len - new_len - 1] == '\\'))
	return;
    }
  if (strchr (sfname, '/') == NULL && strchr (sfname, '\\') == NULL)
    {
      const char *class_name
	= IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class)));
      const char *dot = strrchr (class_name, '.');
      if (dot != NULL)
	{
	  /* Length of prefix, not counting final dot. */
	  int i = dot - class_name;
	  /* Concatenate current package prefix with new sfname. */
	  char *buf = XNEWVEC (char, i + new_len + 2); /* Space for '.' and '\0'. */
	  strcpy (buf + i + 1, sfname);
	  /* Copy package from class_name, replacing '.' by DIR_SEPARATOR.
	     Note we start at the end with the final package dot. */
	  for (; i >= 0;  i--)
	    {
	      char c = class_name[i];
	      if (c == '.')
		c = DIR_SEPARATOR;
	      buf[i] = c;
	    }
	  sfname_id = get_identifier (buf);
	  free (buf);
	  sfname = IDENTIFIER_POINTER (sfname_id);
	}
    }
      
  sfname = find_sourcefile (sfname);
  ORDINARY_MAP_FILE_NAME (LINEMAPS_LAST_ORDINARY_MAP (line_table)) = sfname;
  if (current_class == main_class) main_input_filename = sfname;
}




/* Annotation handling.  

   The technique we use here is to copy the annotation data directly
   from the input class file into the output file.  We don't decode the
   data at all, merely rewriting constant indexes whenever we come
   across them: this is necessary because the constant pool in the
   output file isn't the same as the constant pool in the input.

   The main advantage of this technique is that the resulting
   annotation data is pointer-free, so it doesn't have to be relocated
   at startup time.  As a consequence of this, annotations have no
   performance impact unless they are used.  Also, this representation
   is very dense.  */


/* Expand TYPE_REFLECTION_DATA by DELTA bytes.  Return the address of
   the start of the newly allocated region.  */

static unsigned char*
annotation_grow (int delta)
{
  unsigned char **data = &TYPE_REFLECTION_DATA (current_class);
  long *datasize = &TYPE_REFLECTION_DATASIZE (current_class);
  long len = *datasize;

  if (*data == NULL)
    {
      *data = XNEWVAR (unsigned char, delta);
    }
  else
    {
      int newlen = *datasize + delta;
      if (floor_log2 (newlen) != floor_log2 (*datasize))
	*data = XRESIZEVAR (unsigned char, *data,  2 << (floor_log2 (newlen)));
    }
  *datasize += delta;
  return *data + len;
}

/* annotation_rewrite_TYPE.  Rewrite various int types at p.  Use Java
   byte order (i.e. big endian.)  */

static void
annotation_rewrite_byte (unsigned int n, unsigned char *p)
{
  p[0] = n;
}

static void
annotation_rewrite_short (unsigned int n, unsigned char *p)
{
  p[0] = n>>8;
  p[1] = n;
}

static void
annotation_rewrite_int (unsigned int n, unsigned char *p)
{
  p[0] = n>>24;
  p[1] = n>>16;
  p[2] = n>>8;
  p[3] = n;
}

/* Read a 16-bit unsigned int in Java byte order (i.e. big
   endian.)  */

static uint16
annotation_read_short (unsigned char *p)
{
  uint16 tmp = p[0];
  tmp = (tmp << 8) | p[1];
  return tmp;
}

/* annotation_write_TYPE.  Rewrite various int types, appending them
   to TYPE_REFLECTION_DATA.  Use Java byte order (i.e. big
   endian.)  */

static void
annotation_write_byte (unsigned int n)
{
  annotation_rewrite_byte (n, annotation_grow (1));
}

static void
annotation_write_short (unsigned int n)
{
  annotation_rewrite_short (n, annotation_grow (2));
}

static void
annotation_write_int (unsigned int n)
{
  annotation_rewrite_int (n, annotation_grow (4));
}

/* Create a 64-bit constant in the constant pool.

   This is used for both integer and floating-point types.  As a
   consequence, it will not work if the target floating-point format
   is anything other than IEEE-754.  While this is arguably a bug, the
   runtime library makes exactly the same assumption and it's unlikely
   that Java will ever run on a non-IEEE machine.  */

static int 
handle_long_constant (JCF *jcf, CPool *cpool, enum cpool_tag kind,
		    int index, bool big_endian)
{
  /* If we're on a 64-bit platform we can fit a long or double
     into the same space as a jword.  */
  if (POINTER_SIZE >= 64)
    index = find_constant1 (cpool, kind, JPOOL_LONG (jcf, index));

  /* In a compiled program the constant pool is in native word
     order.  How weird is that???  */
  else if (big_endian)
    index = find_constant2 (cpool, kind,
			    JPOOL_INT (jcf, index), 
			    JPOOL_INT (jcf, index+1));
  else
    index = find_constant2 (cpool, kind,
			    JPOOL_INT (jcf, index+1), 
			    JPOOL_INT (jcf, index));
  
  return index;
}

/* Given a class file and an index into its constant pool, create an
   entry in the outgoing constant pool for the same item.  */

static uint16
handle_constant (JCF *jcf, int index, enum cpool_tag purpose)
{
  unsigned int kind;
  CPool *cpool = cpool_for_class (output_class);
  
  if (index == 0)
    return 0;

  if (! CPOOL_INDEX_IN_RANGE (&jcf->cpool, index))
    error ("<constant pool index %d not in range>", index);
  
  kind = JPOOL_TAG (jcf, index);

  if ((kind & ~CONSTANT_ResolvedFlag) != purpose)
    {
      if (purpose == CONSTANT_Class
	  && kind == CONSTANT_Utf8)
	;
      else
	error ("<constant pool index %d unexpected type", index);
    }

  switch (kind)
    {
    case CONSTANT_Class:
    case CONSTANT_ResolvedClass:
      {
	/* For some reason I know not the what of, class names in
	   annotations are UTF-8 strings in the constant pool but
	   class names in EnclosingMethod attributes are real class
	   references.  Set CONSTANT_LazyFlag here so that the VM
	   doesn't attempt to resolve them at class initialization
	   time.  */
	tree resolved_class, class_name;
	resolved_class = get_class_constant (jcf, index);
	class_name = build_internal_class_name (resolved_class);
	index = alloc_name_constant (CONSTANT_Class | CONSTANT_LazyFlag,
				     (unmangle_classname 
				      (IDENTIFIER_POINTER(class_name),
				       IDENTIFIER_LENGTH(class_name))));
	break;
      }
    case CONSTANT_Utf8:
      {
	tree utf8 = get_constant (jcf, index);
	if (purpose == CONSTANT_Class)
	  /* Create a constant pool entry for a type signature.  This
	     one has '.' rather than '/' because it isn't going into a
	     class file, it's going into a compiled object.
	     
	     This has to match the logic in
	     _Jv_ClassReader::prepare_pool_entry().  */
	  utf8 = unmangle_classname (IDENTIFIER_POINTER(utf8),
				     IDENTIFIER_LENGTH(utf8));
	index = alloc_name_constant (kind, utf8);
      }
      break;

    case CONSTANT_Long:
      index = handle_long_constant (jcf, cpool, CONSTANT_Long, index,
				    targetm.words_big_endian ());
      break;
      
    case CONSTANT_Double:
      index = handle_long_constant (jcf, cpool, CONSTANT_Double, index,
				    targetm.float_words_big_endian ());
      break;

    case CONSTANT_Float:
    case CONSTANT_Integer:
      index = find_constant1 (cpool, kind, JPOOL_INT (jcf, index));
      break;
      
    case CONSTANT_NameAndType:
      {
	uint16 name = JPOOL_USHORT1 (jcf, index);
	uint16 sig = JPOOL_USHORT2 (jcf, index);
	uint32 name_index = handle_constant (jcf, name, CONSTANT_Utf8);
	uint32 sig_index = handle_constant (jcf, sig, CONSTANT_Class);
	jword new_index = (name_index << 16) | sig_index;
	index = find_constant1 (cpool, kind, new_index);
      }
      break;

    default:
      abort ();
    }
  
  return index;
}

/* Read an element_value structure from an annotation in JCF.  Return
   the constant pool index for the resulting constant pool entry.  */

static int
handle_element_value (JCF *jcf, int level)
{
  uint8 tag = JCF_readu (jcf);
  int index = 0;

  annotation_write_byte (tag);
  switch (tag)
    {
    case 'B':
    case 'C':
    case 'S':
    case 'Z':
    case 'I':
      {
	uint16 cindex = JCF_readu2 (jcf);
	index = handle_constant (jcf, cindex,
				 CONSTANT_Integer);
	annotation_write_short (index);
      }
      break;
    case 'D':
      {
	uint16 cindex = JCF_readu2 (jcf);
	index = handle_constant (jcf, cindex,
				 CONSTANT_Double);
	annotation_write_short (index);
      }
      break;
    case 'F':
      {
	uint16 cindex = JCF_readu2 (jcf);
	index = handle_constant (jcf, cindex,
				 CONSTANT_Float);
	annotation_write_short (index);
      }
      break;
    case 'J':
      {
	uint16 cindex = JCF_readu2 (jcf);
	index = handle_constant (jcf, cindex,
				 CONSTANT_Long);
	annotation_write_short (index);
      }
      break;
    case 's':
      {
	uint16 cindex = JCF_readu2 (jcf);
	/* Despite what the JVM spec says, compilers generate a Utf8
	   constant here, not a String.  */
	index = handle_constant (jcf, cindex,
				 CONSTANT_Utf8);
	annotation_write_short (index);
      }
      break;

    case 'e':
      {
	uint16 type_name_index = JCF_readu2 (jcf);
	uint16 const_name_index = JCF_readu2 (jcf);
	index = handle_constant (jcf, type_name_index,
				 CONSTANT_Class);
	annotation_write_short (index);
	index = handle_constant (jcf, const_name_index,
				 CONSTANT_Utf8);
	annotation_write_short (index);
     }
      break;
    case 'c':
      {
	uint16 class_info_index = JCF_readu2 (jcf);
	index = handle_constant (jcf, class_info_index,
				 CONSTANT_Class);
	annotation_write_short (index);
      }
      break;
    case '@':
      {
	handle_annotation (jcf, level + 1);
      }
      break;
    case '[':
      {
	uint16 n_array_elts = JCF_readu2 (jcf);
	annotation_write_short (n_array_elts);
	while (n_array_elts--)
	  handle_element_value (jcf, level + 1);
      }
      break;
    default:
      abort();
      break;
    }
  return index;
}

/* Read an annotation structure from JCF.  Write it to the
   reflection_data field of the outgoing class.  */

static void
handle_annotation (JCF *jcf, int level)
{
  uint16 type_index = JCF_readu2 (jcf);
  uint16 npairs = JCF_readu2 (jcf);
  int index = handle_constant (jcf, type_index,
			       CONSTANT_Class);
  annotation_write_short (index);
  annotation_write_short (npairs);
  while (npairs--)
    {
      uint16 name_index = JCF_readu2 (jcf);
      index = handle_constant (jcf, name_index,
			       CONSTANT_Utf8);
      annotation_write_short (index);
      handle_element_value (jcf, level + 2);
    }
}

/* Read an annotation count from JCF, and write the following
   annotations to the reflection_data field of the outgoing class.  */

static void
handle_annotations (JCF *jcf, int level)
{
  uint16 num = JCF_readu2 (jcf);
  annotation_write_short (num);
  while (num--)
    handle_annotation (jcf, level);
}

/* As handle_annotations(), but perform a sanity check that we write
   the same number of bytes that we were expecting.  */

static void
handle_annotation_attribute (int ATTRIBUTE_UNUSED index, JCF *jcf, 
			     long length)
{
  long old_datasize = TYPE_REFLECTION_DATASIZE (current_class);

  handle_annotations (jcf, 0);

  gcc_assert (old_datasize + length
	      == TYPE_REFLECTION_DATASIZE (current_class));
}

/* gcj permutes its fields array after generating annotation_data, so
   we have to fixup field indexes for fields that have moved.  Given
   ARG, a VEC_int, fixup the field indexes in the reflection_data of
   the outgoing class.  We use field_offsets to tell us where the
   fixups must go.  */

void
rewrite_reflection_indexes (void *arg)
{
  bitmap_iterator bi;
  unsigned int offset;
  vec<int> *map = (vec<int> *) arg;
  unsigned char *data = TYPE_REFLECTION_DATA (current_class);

  if (map)
    {
      EXECUTE_IF_SET_IN_BITMAP (field_offsets, 0, offset, bi)
	{
	  uint16 index = annotation_read_short (data + offset);
	  annotation_rewrite_short 
	    ((*map)[index], data + offset);
	}
    }
}

/* Read the RuntimeVisibleAnnotations from JCF and write them to the
   reflection_data of the outgoing class.  */

static void
handle_member_annotations (int member_index, JCF *jcf, 
			   const unsigned char *name ATTRIBUTE_UNUSED, 
			   long len, jv_attr_type member_type)
{
  int new_len = len + 1;
  annotation_write_byte (member_type);
  if (member_type != JV_CLASS_ATTR)
    new_len += 2;
  annotation_write_int (new_len);
  annotation_write_byte (JV_ANNOTATIONS_KIND);
  if (member_type == JV_FIELD_ATTR)
    bitmap_set_bit (field_offsets, TYPE_REFLECTION_DATASIZE (current_class));
  if (member_type != JV_CLASS_ATTR)
    annotation_write_short (member_index);
  handle_annotation_attribute (member_index, jcf, len);
}

/* Read the RuntimeVisibleParameterAnnotations from JCF and write them
   to the reflection_data of the outgoing class.  */

static void
handle_parameter_annotations (int member_index, JCF *jcf, 
			      const unsigned char *name ATTRIBUTE_UNUSED, 
			      long len, jv_attr_type member_type)
{
  int new_len = len + 1;
  uint8 num;
  annotation_write_byte (member_type);
  if (member_type != JV_CLASS_ATTR)
    new_len += 2;
  annotation_write_int (new_len);
  annotation_write_byte (JV_PARAMETER_ANNOTATIONS_KIND);
  if (member_type != JV_CLASS_ATTR)
    annotation_write_short (member_index);
  num = JCF_readu (jcf);
  annotation_write_byte (num);
  while (num--)
    handle_annotations (jcf, 0);
}


/* Read the AnnotationDefault data from JCF and write them to the
   reflection_data of the outgoing class.  */

static void
handle_default_annotation (int member_index, JCF *jcf, 
			   const unsigned char *name ATTRIBUTE_UNUSED, 
			   long len, jv_attr_type member_type)
{
  int new_len = len + 1;
  annotation_write_byte (member_type);
  if (member_type != JV_CLASS_ATTR)
    new_len += 2;
  annotation_write_int (new_len);
  annotation_write_byte (JV_ANNOTATION_DEFAULT_KIND);
  if (member_type != JV_CLASS_ATTR)
    annotation_write_short (member_index);
  handle_element_value (jcf, 0);
}

/* As above, for the EnclosingMethod attribute.  */

static void
handle_enclosingmethod_attribute (int member_index, JCF *jcf, 
			   const unsigned char *name ATTRIBUTE_UNUSED, 
			   long len, jv_attr_type member_type)
{
  int new_len = len + 1;
  uint16 index;
  annotation_write_byte (member_type);
  if (member_type != JV_CLASS_ATTR)
    new_len += 2;
  annotation_write_int (new_len);
  annotation_write_byte (JV_ENCLOSING_METHOD_KIND);
  if (member_type != JV_CLASS_ATTR)
    annotation_write_short (member_index);

  index = JCF_readu2 (jcf);
  index = handle_constant (jcf, index, CONSTANT_Class);
  annotation_write_short (index);

  index = JCF_readu2 (jcf);
  index = handle_constant (jcf, index, CONSTANT_NameAndType);
  annotation_write_short (index);
}

/* As above, for the Signature attribute.  */

static void
handle_signature_attribute (int member_index, JCF *jcf, 
			   const unsigned char *name ATTRIBUTE_UNUSED, 
			   long len, jv_attr_type member_type)
{
  int new_len = len + 1;
  uint16 index;
  annotation_write_byte (member_type);
  if (member_type != JV_CLASS_ATTR)
    new_len += 2;
  annotation_write_int (new_len);
  annotation_write_byte (JV_SIGNATURE_KIND);
  if (member_type != JV_CLASS_ATTR)
    annotation_write_short (member_index);

  index = JCF_readu2 (jcf);
  index = handle_constant (jcf, index, CONSTANT_Utf8);
  annotation_write_short (index);
}
  


#define HANDLE_SOURCEFILE(INDEX) set_source_filename (jcf, INDEX)

#define HANDLE_CLASS_INFO(ACCESS_FLAGS, THIS, SUPER, INTERFACES_COUNT) \
{ tree super_class = SUPER==0 ? NULL_TREE : get_class_constant (jcf, SUPER); \
  output_class = current_class = give_name_to_class (jcf, THIS); \
  set_super_info (ACCESS_FLAGS, current_class, super_class, INTERFACES_COUNT);}

#define HANDLE_CLASS_INTERFACE(INDEX) \
  add_interface (current_class, get_class_constant (jcf, INDEX))

#define HANDLE_START_FIELD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
{ int sig_index = SIGNATURE; \
  current_field = add_field (current_class, get_name_constant (jcf, NAME), \
			     parse_signature (jcf, sig_index), ACCESS_FLAGS); \
 set_java_signature (TREE_TYPE (current_field), JPOOL_UTF (jcf, sig_index)); \
 if ((ACCESS_FLAGS) & ACC_FINAL) \
   MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (current_field); \
}

#define HANDLE_END_FIELDS() \
  (current_field = NULL_TREE)

#define HANDLE_CONSTANTVALUE(INDEX) \
{ tree constant;  int index = INDEX; \
  if (JPOOL_TAG (jcf, index) == CONSTANT_String) { \
    tree name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index)); \
    constant = build_utf8_ref (name); \
  } \
  else \
    constant = get_constant (jcf, index); \
  set_constant_value (current_field, constant); }

#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
 (current_method = add_method (current_class, ACCESS_FLAGS, \
			       get_name_constant (jcf, NAME), \
			       get_name_constant (jcf, SIGNATURE)), \
  DECL_LOCALVARIABLES_OFFSET (current_method) = 0, \
  DECL_LINENUMBERS_OFFSET (current_method) = 0)

#define HANDLE_END_METHODS() \
{ current_method = NULL_TREE; }

#define HANDLE_CODE_ATTRIBUTE(MAX_STACK, MAX_LOCALS, CODE_LENGTH) \
{ DECL_MAX_STACK (current_method) = (MAX_STACK); \
  DECL_MAX_LOCALS (current_method) = (MAX_LOCALS); \
  DECL_CODE_LENGTH (current_method) = (CODE_LENGTH); \
  DECL_CODE_OFFSET (current_method) = JCF_TELL (jcf); }

#define HANDLE_LOCALVARIABLETABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); \
  DECL_LOCALVARIABLES_OFFSET (current_method) = JCF_TELL (jcf) - 2; \
  JCF_SKIP (jcf, n * 10); }

#define HANDLE_LINENUMBERTABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); \
  DECL_LINENUMBERS_OFFSET (current_method) = JCF_TELL (jcf) - 2; \
  JCF_SKIP (jcf, n * 4); }

#define HANDLE_EXCEPTIONS_ATTRIBUTE(COUNT) \
{ \
  int n = COUNT; \
  vec<tree, va_gc> *v; \
  vec_alloc (v, n); \
  gcc_assert (!DECL_FUNCTION_THROWS (current_method)); \
  while (--n >= 0) \
    { \
      tree thrown_class = get_class_constant (jcf, JCF_readu2 (jcf)); \
      v->quick_push (thrown_class); \
    } \
  DECL_FUNCTION_THROWS (current_method) = v; \
}

#define HANDLE_DEPRECATED_ATTRIBUTE()  handle_deprecated ()

/* Link seen inner classes to their outer context and register the
   inner class to its outer context. They will be later loaded.  */
#define HANDLE_INNERCLASSES_ATTRIBUTE(COUNT) \
  handle_innerclass_attribute (COUNT, jcf, attribute_length)

#define HANDLE_SYNTHETIC_ATTRIBUTE()					\
{									\
  /* Irrelevant decls should have been nullified by the END macros.	\
     DECL_ARTIFICIAL on fields is used for something else (See		\
     PUSH_FIELD in java-tree.h) */					\
  if (current_method)							\
    DECL_ARTIFICIAL (current_method) = 1;				\
  else if (current_field)						\
    FIELD_SYNTHETIC (current_field) = 1;				\
  else									\
    TYPE_SYNTHETIC (current_class) = 1;					\
}

#define HANDLE_GCJCOMPILED_ATTRIBUTE()		\
{ 						\
  if (current_class == object_type_node)	\
    jcf->right_zip = 1;				\
}

#define HANDLE_RUNTIMEVISIBLEANNOTATIONS_ATTRIBUTE()			\
{									\
  handle_member_annotations (index, jcf, name_data, attribute_length, attr_type); \
}

#define HANDLE_RUNTIMEINVISIBLEANNOTATIONS_ATTRIBUTE()	\
{							\
  JCF_SKIP(jcf, attribute_length);			\
}

#define HANDLE_RUNTIMEVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE()		\
{									\
  handle_parameter_annotations (index, jcf, name_data, attribute_length, attr_type); \
}

#define HANDLE_RUNTIMEINVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE()	\
{								\
  JCF_SKIP(jcf, attribute_length);				\
}

#define HANDLE_ANNOTATIONDEFAULT_ATTRIBUTE()				\
{									\
  handle_default_annotation (index, jcf, name_data, attribute_length, attr_type); \
}

#define HANDLE_ENCLOSINGMETHOD_ATTRIBUTE()				\
{									\
  handle_enclosingmethod_attribute (index, jcf, name_data,		\
				    attribute_length, attr_type);	\
}

#define HANDLE_SIGNATURE_ATTRIBUTE()				\
{								\
  handle_signature_attribute (index, jcf, name_data,		\
			      attribute_length, attr_type);	\
}

#include "jcf-reader.c"

tree
parse_signature (JCF *jcf, int sig_index)
{
  gcc_assert (sig_index > 0
	      && sig_index < JPOOL_SIZE (jcf)
	      && JPOOL_TAG (jcf, sig_index) == CONSTANT_Utf8);

  return parse_signature_string (JPOOL_UTF_DATA (jcf, sig_index),
				 JPOOL_UTF_LENGTH (jcf, sig_index));
}

tree
get_constant (JCF *jcf, int index)
{
  tree value;
  int tag;
  if (index <= 0 || index >= JPOOL_SIZE(jcf))
    goto bad;
  tag = JPOOL_TAG (jcf, index);
  if ((tag & CONSTANT_ResolvedFlag) || tag == CONSTANT_Utf8)
    return jcf->cpool.data[index].t;
  switch (tag)
    {
    case CONSTANT_Integer:
      {
	jint num = JPOOL_INT(jcf, index);
	value = build_int_cst (int_type_node, num);
	break;
      }
    case CONSTANT_Long:
      {
	unsigned HOST_WIDE_INT num;
	double_int val;

	num = JPOOL_UINT (jcf, index);
	val = double_int::from_uhwi (num).llshift (32, 64);
	num = JPOOL_UINT (jcf, index + 1);
	val |= double_int::from_uhwi (num);

	value = double_int_to_tree (long_type_node, val);
	break;
      }

    case CONSTANT_Float:
      {
	jint num = JPOOL_INT(jcf, index);
	long buf = num;
	REAL_VALUE_TYPE d;

	real_from_target_fmt (&d, &buf, &ieee_single_format);
	value = build_real (float_type_node, d);
	break;
      }

    case CONSTANT_Double:
      {
	long buf[2], lo, hi;
	REAL_VALUE_TYPE d;

	hi = JPOOL_UINT (jcf, index);
	lo = JPOOL_UINT (jcf, index+1);

	if (targetm.float_words_big_endian ())
	  buf[0] = hi, buf[1] = lo;
	else
	  buf[0] = lo, buf[1] = hi;

	real_from_target_fmt (&d, buf, &ieee_double_format);
	value = build_real (double_type_node, d);
	break;
      }

    case CONSTANT_String:
      {
	tree name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index));
	const char *utf8_ptr = IDENTIFIER_POINTER (name);
	int utf8_len = IDENTIFIER_LENGTH (name);
	const unsigned char *utf8;
	int i;

	/* Check for a malformed Utf8 string.  */
	utf8 = (const unsigned char *) utf8_ptr;
	i = utf8_len;
	while (i > 0)
	  {
	    int char_len = UT8_CHAR_LENGTH (*utf8);
	    if (char_len < 0 || char_len > 3 || char_len > i)
 	      fatal_error ("bad string constant");

	    utf8 += char_len;
	    i -= char_len;
	  }

	/* Allocate a new string value.  */
	value = build_string (utf8_len, utf8_ptr);
	TREE_TYPE (value) = build_pointer_type (string_type_node);
      }
      break;
    default:
      goto bad;
    }
  JPOOL_TAG (jcf, index) = tag | CONSTANT_ResolvedFlag;
  jcf->cpool.data[index].t = value;
  return value;
 bad:
  fatal_error ("bad value constant type %d, index %d", 
	       JPOOL_TAG (jcf, index), index);
}

tree
get_name_constant (JCF *jcf, int index)
{
  tree name = get_constant (jcf, index);
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);
  return name;
}

/* Handle reading innerclass attributes. If a nonzero entry (denoting
   a non anonymous entry) is found, We augment the inner class list of
   the outer context with the newly resolved innerclass.  */

static void
handle_innerclass_attribute (int count, JCF *jcf, int attribute_length)
{
  int c = count;

  annotation_write_byte (JV_CLASS_ATTR);
  annotation_write_int (attribute_length+1);
  annotation_write_byte (JV_INNER_CLASSES_KIND);
  annotation_write_short (count);

  while (c--)
    {
      /* Read inner_class_info_index. This may be 0 */
      int icii = JCF_readu2 (jcf);
      /* Read outer_class_info_index. If the innerclasses attribute
	 entry isn't a member (like an inner class) the value is 0. */
      int ocii = JCF_readu2 (jcf);
      /* Read inner_name_index. If the class we're dealing with is
	 an anonymous class, it must be 0. */
      int ini = JCF_readu2 (jcf);
      /* Read the access flag. */
      int acc = JCF_readu2 (jcf);

      annotation_write_short (handle_constant (jcf, icii, CONSTANT_Class));
      annotation_write_short (handle_constant (jcf, ocii, CONSTANT_Class));
      annotation_write_short (handle_constant (jcf, ini, CONSTANT_Utf8));
      annotation_write_short (acc);

      /* If icii is 0, don't try to read the class. */
      if (icii >= 0)
	{
	  tree klass = get_class_constant (jcf, icii);
	  tree decl = TYPE_NAME (klass);
          /* Skip reading further if ocii is null */
          if (DECL_P (decl) && !CLASS_COMPLETE_P (decl) && ocii)
	    {
	      tree outer = TYPE_NAME (get_class_constant (jcf, ocii));
	      tree alias = (ini ? get_name_constant (jcf, ini) : NULL_TREE);
	      set_class_decl_access_flags (acc, decl);
	      DECL_CONTEXT (decl) = outer;
	      DECL_INNER_CLASS_LIST (outer) =
		tree_cons (decl, alias, DECL_INNER_CLASS_LIST (outer));
	      CLASS_COMPLETE_P (decl) = 1;
            }
	}
    }
}

static tree
give_name_to_class (JCF *jcf, int i)
{
  gcc_assert (i > 0
	      && i < JPOOL_SIZE (jcf)
	      && JPOOL_TAG (jcf, i) == CONSTANT_Class);

    {
      tree package_name = NULL_TREE, tmp;
      tree this_class;
      int j = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that j is a CONSTANT_Utf8. */
      tree class_name = unmangle_classname ((const char *) JPOOL_UTF_DATA (jcf, j),
					    JPOOL_UTF_LENGTH (jcf, j));
      this_class = lookup_class (class_name);
      {
      tree source_name = identifier_subst (class_name, "", '.', '/', ".java");
      const char *sfname = find_sourcefile (IDENTIFIER_POINTER (source_name));
      linemap_add (line_table, LC_ENTER, false, sfname, 0);
      input_location = linemap_line_start (line_table, 0, 1);
      file_start_location = input_location;
      DECL_SOURCE_LOCATION (TYPE_NAME (this_class)) = input_location;
      if (main_input_filename == NULL && jcf == main_jcf)
	main_input_filename = sfname;
      }

      jcf->cpool.data[i].t = this_class;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
      split_qualified_name (&package_name, &tmp, 
      			    DECL_NAME (TYPE_NAME (this_class)));
      TYPE_PACKAGE (this_class) = package_name;
      return this_class;
    }
}

/* Get the class of the CONSTANT_Class whose constant pool index is I. */

tree
get_class_constant (JCF *jcf, int i)
{
  tree type;
  gcc_assert (i > 0
	      && i < JPOOL_SIZE (jcf)
	      && (JPOOL_TAG (jcf, i) & ~CONSTANT_ResolvedFlag) == CONSTANT_Class);

  if (JPOOL_TAG (jcf, i) != CONSTANT_ResolvedClass)
    {
      int name_index = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that name_index is a CONSTANT_Utf8. */
      const char *name = (const char *) JPOOL_UTF_DATA (jcf, name_index);
      int nlength = JPOOL_UTF_LENGTH (jcf, name_index);

      if (name[0] == '[')  /* Handle array "classes". */
	  type = TREE_TYPE (parse_signature_string ((const unsigned char *) name, nlength));
      else
        { 
          tree cname = unmangle_classname (name, nlength);
          type = lookup_class (cname);
	}
      jcf->cpool.data[i].t = type;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
    }
  else
    type = jcf->cpool.data[i].t;
  return type;
}

/* Read a class with the fully qualified-name NAME.
   Return 1 iff we read the requested file.
   (It is still possible we failed if the file did not
   define the class it is supposed to.) */

int
read_class (tree name)
{
  JCF this_jcf, *jcf;
  tree icv, klass = NULL_TREE;
  tree save_current_class = current_class;
  tree save_output_class = output_class;
  location_t save_location = input_location;
  JCF *save_current_jcf = current_jcf;

  if ((icv = IDENTIFIER_CLASS_VALUE (name)) != NULL_TREE)
    {
      klass = TREE_TYPE (icv);
      jcf = TYPE_JCF (klass);
    }
  else
    jcf = NULL;

  if (jcf == NULL)
    {
      const char* path_name;
      this_jcf.zipd = NULL;
      jcf = &this_jcf;
      
      path_name = find_class (IDENTIFIER_POINTER (name),
			      IDENTIFIER_LENGTH (name),
			      &this_jcf);
      if (path_name == 0)
	return 0;
      else
	free(CONST_CAST (char *, path_name));
    }

  current_jcf = jcf;

  if (klass == NULL_TREE || ! CLASS_PARSED_P (klass))
    {
      output_class = current_class = klass;
      if (JCF_SEEN_IN_ZIP (current_jcf))
	read_zip_member(current_jcf,
			current_jcf->zipd, current_jcf->zipd->zipf);
      jcf_parse (current_jcf);
      /* Parsing might change the class, in which case we have to
	 put it back where we found it.  */
      if (current_class != klass && icv != NULL_TREE)
	TREE_TYPE (icv) = current_class;
      klass = current_class;
    }
  layout_class (klass);
  load_inner_classes (klass);

  output_class = save_output_class;
  current_class = save_current_class;
  input_location = save_location;
  current_jcf = save_current_jcf;
  return 1;
}

/* Load CLASS_OR_NAME. CLASS_OR_NAME can be a mere identifier if
   called from the parser, otherwise it's a RECORD_TYPE node. If
   VERBOSE is 1, print error message on failure to load a class. */
void
load_class (tree class_or_name, int verbose)
{
  tree name, saved;
  int class_loaded = 0;
  tree class_decl = NULL_TREE;
  bool is_compiled_class = false;

  /* We've already failed, don't try again.  */
  if (TREE_CODE (class_or_name) == RECORD_TYPE
      && TYPE_DUMMY (class_or_name))
    return;

  /* class_or_name can be the name of the class we want to load */
  if (TREE_CODE (class_or_name) == IDENTIFIER_NODE)
    name = class_or_name;
  /* In some cases, it's a dependency that we process earlier that
     we though */
  else if (TREE_CODE (class_or_name) == TREE_LIST)
    name = TYPE_NAME (TREE_PURPOSE (class_or_name));
  /* Or it's a type in the making */
  else
    name = DECL_NAME (TYPE_NAME (class_or_name));

  class_decl = IDENTIFIER_CLASS_VALUE (name);
  if (class_decl != NULL_TREE)
    {
      tree type = TREE_TYPE (class_decl);
      is_compiled_class
	= ((TYPE_JCF (type) && JCF_SEEN_IN_ZIP (TYPE_JCF (type)))
	   || CLASS_FROM_CURRENTLY_COMPILED_P (type));
    }

  saved = name;
  
  /* If flag_verify_invocations is unset, we don't try to load a class
     unless we're looking for Object (which is fixed by the ABI) or
     it's a class that we're going to compile.  */
  if (flag_verify_invocations
      || class_or_name == object_type_node
      || is_compiled_class
      || TREE_CODE (class_or_name) == IDENTIFIER_NODE)
    {
      while (1)
	{
	  const char *separator;

	  /* We've already loaded it.  */
	  if (IDENTIFIER_CLASS_VALUE (name) != NULL_TREE)
	    {
	      tree tmp_decl = IDENTIFIER_CLASS_VALUE (name);
	      if (CLASS_PARSED_P (TREE_TYPE (tmp_decl)))
		break;
	    }
	
	  if (read_class (name))
	    break;

	  /* We failed loading name. Now consider that we might be looking
	     for an inner class.  */
	  if ((separator = strrchr (IDENTIFIER_POINTER (name), '$'))
	      || (separator = strrchr (IDENTIFIER_POINTER (name), '.')))
	    name = get_identifier_with_length (IDENTIFIER_POINTER (name),
					       (separator
						- IDENTIFIER_POINTER (name)));
	  /* Otherwise, we failed, we bail. */
	  else
	    break;
	}

      {
	/* have we found the class we're looking for?  */
	tree type_decl = IDENTIFIER_CLASS_VALUE (saved);
	tree type = type_decl ? TREE_TYPE (type_decl) : NULL;
	class_loaded = type && CLASS_PARSED_P (type);
      }	      
    }
  
  if (!class_loaded)
    {
      if (flag_verify_invocations || ! flag_indirect_dispatch)
	{
	  if (verbose)
	    error ("cannot find file for class %s", IDENTIFIER_POINTER (saved));
	}
      else if (verbose)
	{
	  /* This is just a diagnostic during testing, not a real problem.  */
	  if (!quiet_flag)
	    warning (0, "cannot find file for class %s", 
		     IDENTIFIER_POINTER (saved));
	  
	  /* Fake it.  */
	  if (TREE_CODE (class_or_name) == RECORD_TYPE)
	    {
	      set_super_info (0, class_or_name, object_type_node, 0);
	      TYPE_DUMMY (class_or_name) = 1;
	      /* We won't be able to output any debug info for this class.  */
	      DECL_IGNORED_P (TYPE_NAME (class_or_name)) = 1;
	    }
	}
    }
}

/* Parse the .class file JCF. */

static void
jcf_parse (JCF* jcf)
{
  int i, code;

  bitmap_clear (field_offsets);

  if (jcf_parse_preamble (jcf) != 0)
    fatal_error ("not a valid Java .class file");
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    fatal_error ("error while parsing constant pool");
  code = verify_constant_pool (jcf);
  if (code > 0)
    fatal_error ("error in constant pool entry #%d\n", code);

  jcf_parse_class (jcf);
  if (main_class == NULL_TREE)
    main_class = current_class;
  if (! quiet_flag && TYPE_NAME (current_class))
    fprintf (stderr, " %s %s",
	     (jcf->access_flags & ACC_INTERFACE) ? "interface" : "class", 
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))));
  if (CLASS_PARSED_P (current_class))
    {
      /* FIXME - where was first time */
      fatal_error ("reading class %s for the second time from %s",
		   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))),
		   jcf->filename);
    }
  CLASS_PARSED_P (current_class) = 1;

  for (i = 1; i < JPOOL_SIZE(jcf); i++)
    {
      switch (JPOOL_TAG (jcf, i))
	{
	case CONSTANT_Class:
	  get_class_constant (jcf, i);
	  break;
	}
    }
  
  code = jcf_parse_fields (jcf);
  if (code != 0)
    fatal_error ("error while parsing fields");
  code = jcf_parse_methods (jcf);
  if (code != 0)
    fatal_error ("error while parsing methods");
  code = jcf_parse_final_attributes (jcf);
  if (code != 0)
    fatal_error ("error while parsing final attributes");

  if (TYPE_REFLECTION_DATA (current_class))
    annotation_write_byte (JV_DONE_ATTR);

  linemap_add (line_table, LC_LEAVE, false, NULL, 0);

  /* The fields of class_type_node are already in correct order. */
  if (current_class != class_type_node && current_class != object_type_node)
    TYPE_FIELDS (current_class) = nreverse (TYPE_FIELDS (current_class));

  if (current_class == object_type_node)
    layout_class_methods (object_type_node);
  else
    vec_safe_push (all_class_list, TYPE_NAME (current_class));
}

/* If we came across inner classes, load them now. */
static void
load_inner_classes (tree cur_class)
{
  tree current;
  for (current = DECL_INNER_CLASS_LIST (TYPE_NAME (cur_class)); current;
       current = TREE_CHAIN (current))
    {
      tree name = DECL_NAME (TREE_PURPOSE (current));
      tree decl = IDENTIFIER_GLOBAL_VALUE (name);
      if (decl && ! CLASS_LOADED_P (TREE_TYPE (decl))
	  && !CLASS_BEING_LAIDOUT (TREE_TYPE (decl)))
	load_class (name, 1);
    }
}

static void
duplicate_class_warning (const char *filename)
{
  location_t warn_loc;
  linemap_add (line_table, LC_RENAME, 0, filename, 0);
  warn_loc = linemap_line_start (line_table, 0, 1);
  warning_at (warn_loc, 0, "duplicate class will only be compiled once");
}

static void
java_layout_seen_class_methods (void)
{
  unsigned start = 0;
  unsigned end = vec_safe_length (all_class_list);

  while (1)
    {
      unsigned ix;
      unsigned new_length;

      for (ix = start; ix != end; ix++)
        {
	  tree decl = (*all_class_list)[ix];
          tree cls = TREE_TYPE (decl);

	  input_location = DECL_SOURCE_LOCATION (decl);

          if (! CLASS_LOADED_P (cls))
            load_class (cls, 0);

          layout_class_methods (cls);
        }

      /* Note that new classes might have been added while laying out
         methods, changing the value of all_class_list.  */
      new_length = vec_safe_length (all_class_list);
      if (end != new_length)
	{
	  start = end;
	  end = new_length;
	}
      else
	break;
    }
}

static void
parse_class_file (void)
{
  tree method;
  location_t save_location = input_location;

  java_layout_seen_class_methods ();

  input_location = DECL_SOURCE_LOCATION (TYPE_NAME (current_class));
  {
    /* Re-enter the current file.  */
    expanded_location loc = expand_location (input_location);
    linemap_add (line_table, LC_ENTER, 0, loc.file, loc.line);
  }
  file_start_location = input_location;
  (*debug_hooks->start_source_file) (input_line, input_filename);

  java_mark_class_local (current_class);

  gen_indirect_dispatch_tables (current_class);

  for (method = TYPE_METHODS (current_class);
       method != NULL_TREE; method = DECL_CHAIN (method))
    {
      JCF *jcf = current_jcf;

      if (METHOD_ABSTRACT (method) || METHOD_DUMMY (method))
	continue;

      if (METHOD_NATIVE (method))
	{
	  tree arg;
	  int  decl_max_locals;

	  if (! flag_jni)
	    continue;
	  /* We need to compute the DECL_MAX_LOCALS. We need to take
             the wide types into account too. */
	  for (arg = TYPE_ARG_TYPES (TREE_TYPE (method)), decl_max_locals = 0; 
	       arg != end_params_node;
	       arg = TREE_CHAIN (arg), decl_max_locals += 1)
	    {
	      if (TREE_VALUE (arg) && TYPE_IS_WIDE (TREE_VALUE (arg)))
		decl_max_locals += 1;
	    }
	  DECL_MAX_LOCALS (method) = decl_max_locals;
	  start_java_method (method);
	  give_name_to_locals (jcf);
	  *get_stmts () = build_jni_stub (method);
	  end_java_method ();
	  continue;
	}

      if (DECL_CODE_OFFSET (method) == 0)
	{
	  current_function_decl = method;
	  error ("missing Code attribute");
	  continue;
	}

      input_location = DECL_SOURCE_LOCATION (TYPE_NAME (current_class));
      if (DECL_LINENUMBERS_OFFSET (method))
	{
	  int i;
	  int min_line = 0;
	  unsigned char *ptr;
	  JCF_SEEK (jcf, DECL_LINENUMBERS_OFFSET (method));
	  linenumber_count = i = JCF_readu2 (jcf);
	  linenumber_table = ptr = jcf->read_ptr;

	  for (ptr += 2; --i >= 0; ptr += 4)
	    {
	      int line = GET_u2 (ptr);
	      /* Set initial input_line to smallest linenumber.
	       * Needs to be set before init_function_start. */
	      if (min_line == 0 || line < min_line)
		min_line = line;
	    }
	  if (min_line != 0)
	    input_location = linemap_line_start (line_table, min_line, 1);
	}
      else
	{
	  linenumber_table = NULL;
	  linenumber_count = 0;
	}

      start_java_method (method);

      note_instructions (jcf, method);

      give_name_to_locals (jcf);

      /* Bump up start_label_pc_this_method so we get a unique label number
	 and reset highest_label_pc_this_method. */
      if (highest_label_pc_this_method >= 0)
	{
	  /* We adjust to the next multiple of 1000.  This is just a frill
	     so the last 3 digits of the label number match the bytecode
	     offset, which might make debugging marginally more convenient. */
	  start_label_pc_this_method
	    = ((((start_label_pc_this_method + highest_label_pc_this_method)
		 / 1000)
		+ 1)
	       * 1000);
	  highest_label_pc_this_method = -1;
	}

      /* Convert bytecode to trees.  */
      expand_byte_code (jcf, method);

      end_java_method ();
    }

  finish_class ();

  (*debug_hooks->end_source_file) (LOCATION_LINE (save_location));
  input_location = save_location;
}

static vec<tree, va_gc> *predefined_filenames;

void
add_predefined_file (tree name)
{
  vec_safe_push (predefined_filenames, name);
}

int
predefined_filename_p (tree node)
{
  unsigned ix;
  tree f;

  FOR_EACH_VEC_SAFE_ELT (predefined_filenames, ix, f)
    if (f == node)
      return 1;

  return 0;
}

/* Generate a function that does all static initialization for this 
   translation unit.  */

static void
java_emit_static_constructor (void)
{
  tree body = NULL;

  emit_register_classes (&body);
  write_resource_constructor (&body);

  if (body)
    {
      tree name = get_identifier ("_Jv_global_static_constructor");

      tree decl 
	= build_decl (input_location, FUNCTION_DECL, name,
		      build_function_type_list (void_type_node, NULL_TREE));

      tree resdecl = build_decl (input_location,
				 RESULT_DECL, NULL_TREE, void_type_node);
      DECL_ARTIFICIAL (resdecl) = 1;
      DECL_RESULT (decl) = resdecl;
      current_function_decl = decl;
      allocate_struct_function (decl, false);

      TREE_STATIC (decl) = 1;
      TREE_USED (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;
      DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;
      DECL_SAVED_TREE (decl) = body;
      DECL_UNINLINABLE (decl) = 1;

      DECL_INITIAL (decl) = make_node (BLOCK);
      TREE_USED (DECL_INITIAL (decl)) = 1;

      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      java_genericize (decl);
      cgraph_finalize_function (decl, false);
    }
}


void
java_parse_file (void)
{
  int filename_count = 0;
  location_t save_location = input_location;
  char *file_list = NULL, *list, *next;
  tree node;
  FILE *finput = NULL;
  int in_quotes = 0;
  unsigned ix;
 
  bitmap_obstack_initialize (&bit_obstack);
  field_offsets = BITMAP_ALLOC (&bit_obstack);

  if (flag_filelist_file)
    {
      int avail = 2000;
      finput = fopen (main_input_filename, "r");
      if (finput == NULL)
	fatal_error ("can%'t open %s: %m", input_filename);
      list = XNEWVEC (char, avail);
      next = list;
      for (;;)
	{
	  int count;
	  if (avail < 500)
	    {
	      count = next - list;
	      avail = 2 * (count + avail);
	      list = XRESIZEVEC (char, list, avail);
	      next = list + count;
	      avail = avail - count;
	    }
	  /* Subtract one to guarantee space for final '\0'. */
	  count = fread (next, 1, avail - 1, finput);
	  if (count == 0)
	    {
	      if (! feof (finput))
		fatal_error ("error closing %s: %m", input_filename);
	      *next = '\0';
	      break;
	    }
	  avail -= count;
	  next += count;
	}
      fclose (finput);
      finput = NULL;
      file_list = list;
    }
  else
    list = CONST_CAST (char *, main_input_filename);

  while (list)
    {
      for (next = list; ; )
	{
	  char ch = *next;
	  if (flag_filelist_file && ! in_quotes
	      && (ch == '\n' || ch == '\r' || ch == '\t' || ch == ' '
		  || ch == '&') /* FIXME */)
	    {
	      if (next == list)
		{
		  next++;
		  list = next;
		  continue;
		}
	      else
		{
		  *next++ = '\0';
		  break;
		}
	    }
	  if (flag_filelist_file && ch == '"')
	    {
	      in_quotes = ! in_quotes;
	      *next++ = '\0';
	      if (in_quotes) 
		list = next;
	      else 
		break;
	    }
	  if (ch == '\0')
	    {
	      next = NULL;
	      break;
	    }
	  next++;
	}

      /* Exclude .java files.  */
      if (strlen (list) > 5 && ! strcmp (list + strlen (list) - 5, ".java"))
	{
	  /* Nothing. */
	}
      else if (list[0]) 
	{
	  node = get_identifier (list);

	  filename_count++;

	  /* Exclude file that we see twice on the command line. */
	     
	  if (IS_A_COMMAND_LINE_FILENAME_P (node))
	    duplicate_class_warning (IDENTIFIER_POINTER (node));
	  else
	    {
	      build_translation_unit_decl (node);
	      IS_A_COMMAND_LINE_FILENAME_P (node) = 1;
	    }
	}
      list = next;
    }

  free (file_list);

  if (filename_count == 0)
    warning (0, "no input file specified");

  if (resource_name)
    {
      const char *resource_filename;
      
      /* Only one resource file may be compiled at a time.  */
      gcc_assert (all_translation_units->length () == 1);

      resource_filename
	= IDENTIFIER_POINTER (DECL_NAME ((*all_translation_units)[0]));
      compile_resource_file (resource_name, resource_filename);

      goto finish;
    }

  current_jcf = main_jcf;
  FOR_EACH_VEC_ELT (*all_translation_units, ix, node)
    {
      unsigned char magic_string[4];
      char *real_path;
      uint32 magic = 0;
      tree name = DECL_NAME (node);
      tree real_file;
      const char *filename = IDENTIFIER_POINTER (name);

      /* Skip already parsed files */
      real_path = lrealpath (filename);
      real_file = get_identifier (real_path);
      free (real_path);
      if (HAS_BEEN_ALREADY_PARSED_P (real_file))
	continue;

      /* Close previous descriptor, if any */
      if (finput && fclose (finput))
	fatal_error ("can%'t close input file %s: %m", main_input_filename);
      
      finput = fopen (filename, "rb");
      if (finput == NULL)
	fatal_error ("can%'t open %s: %m", filename);

#ifdef IO_BUFFER_SIZE
      setvbuf (finput, xmalloc (IO_BUFFER_SIZE),
	       _IOFBF, IO_BUFFER_SIZE);
#endif

      /* Figure what kind of file we're dealing with */
      if (fread (magic_string, 1, 4, finput) == 4)
	{
	  fseek (finput, 0L, SEEK_SET);
	  magic = GET_u4 (magic_string);
	}
      if (magic == 0xcafebabe)
	{
	  CLASS_FILE_P (node) = 1;
	  current_jcf = ggc_alloc_cleared_JCF ();
	  current_jcf->read_state = finput;
	  current_jcf->filbuf = jcf_filbuf_from_stdio;
	  jcf_parse (current_jcf);
	  DECL_SOURCE_LOCATION (node) = file_start_location;
	  TYPE_JCF (current_class) = current_jcf;
	  if (CLASS_FROM_CURRENTLY_COMPILED_P (current_class))
	    {
	      /* We've already compiled this class.  */
	      duplicate_class_warning (filename);
	      continue;
	    }
	  CLASS_FROM_CURRENTLY_COMPILED_P (current_class) = 1;
	  TREE_TYPE (node) = current_class;
	}
      else if (magic == (JCF_u4)ZIPMAGIC)
	{
	  main_jcf = ggc_alloc_cleared_JCF ();
	  main_jcf->read_state = finput;
	  main_jcf->filbuf = jcf_filbuf_from_stdio;
	  linemap_add (line_table, LC_ENTER, false, filename, 0);
	  input_location = linemap_line_start (line_table, 0, 1);
	  if (open_in_zip (main_jcf, filename, NULL, 0) <  0)
	    fatal_error ("bad zip/jar file %s", filename);
	  localToFile = SeenZipFiles;
	  /* Register all the classes defined there.  */
	  process_zip_dir ((FILE *) main_jcf->read_state);
	  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
	  parse_zip_file_entries ();
	}
      else if (magic == (JCF_u4) ZIPEMPTYMAGIC)
	{
	  /* Ignore an empty input jar.  */
	}
      else
	{
	  gcc_unreachable ();
#if 0
	  java_push_parser_context ();
	  java_parser_context_save_global ();

	  parse_source_file_1 (real_file, filename, finput);
	  java_parser_context_restore_global ();
	  java_pop_parser_context (1);
	  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
#endif
	}
    }

  FOR_EACH_VEC_ELT (*all_translation_units, ix, node)
    {
      input_location = DECL_SOURCE_LOCATION (node);
      if (CLASS_FILE_P (node))
	{
	  /* FIXME: These two flags really should be independent.  We
	     should be able to compile fully binary compatible, but
	     with flag_verify_invocations on.  */
	  flag_verify_invocations = ! flag_indirect_dispatch;
	  output_class = current_class = TREE_TYPE (node);

	  current_jcf = TYPE_JCF (current_class);
	  layout_class (current_class);
	  load_inner_classes (current_class);
	  parse_class_file ();
	  JCF_FINISH (current_jcf);
	}
    }
  input_location = save_location;

  bitmap_obstack_release (&bit_obstack);

 finish:
  /* Arrange for any necessary initialization to happen.  */
  java_emit_static_constructor ();
  gcc_assert (global_bindings_p ());
}


/* Return the name of the class corresponding to the name of the file
   in this zip entry.  The result is newly allocated using ALLOC.  */
static char *
compute_class_name (struct ZipDirectory *zdir)
{
  char *class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);
  char *class_name;
  int i;
  int filename_length = zdir->filename_length;

  while (filename_length > 2 && strncmp (class_name_in_zip_dir, "./", 2) == 0)
    {
      class_name_in_zip_dir += 2;
      filename_length -= 2;
    }

  filename_length -= strlen (".class");
  class_name = XNEWVEC (char, filename_length + 1);
  memcpy (class_name, class_name_in_zip_dir, filename_length);
  class_name [filename_length] = '\0';

  for (i = 0; i < filename_length; i++)
    if (class_name[i] == '/')
      class_name[i] = '.';

  return class_name;
}

/* Return 0 if we should skip this entry, 1 if it is a .class file, 2
   if it is a property file of some sort.  */
static int
classify_zip_file (struct ZipDirectory *zdir)
{
  char *class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

  if (zdir->filename_length > 6
      && !strncmp (&class_name_in_zip_dir[zdir->filename_length - 6],
		   ".class", 6))
    return 1;

  /* For now we drop the manifest, but not other information.  */
  if (zdir->filename_length == 20
      && !strncmp (class_name_in_zip_dir, "META-INF/MANIFEST.MF", 20))
    return 0;

  /* Drop directory entries.  */
  if (zdir->filename_length > 0
      && class_name_in_zip_dir[zdir->filename_length - 1] == '/')
    return 0;

  return 2;
}

/* Process all class entries found in the zip file.  */
static void
parse_zip_file_entries (void)
{
  struct ZipDirectory *zdir;
  int i;

  for (i = 0, zdir = (ZipDirectory *)localToFile->central_directory;
       i < localToFile->count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      tree klass;

      switch (classify_zip_file (zdir))
	{
	case 0:
	  continue;

	case 1:
	  {
	    char *class_name = compute_class_name (zdir);
	    int previous_alias_set = -1;
	    klass = lookup_class (get_identifier (class_name));
	    FREE (class_name);
	    current_jcf = TYPE_JCF (klass);
	    output_class = current_class = klass;

	    /* This is a dummy class, and now we're compiling it for
	       real.  */
	    gcc_assert (! TYPE_DUMMY (klass));

	    /* This is for a corner case where we have a superclass
	       but no superclass fields.

	       This can happen if we earlier failed to lay out this
	       class because its superclass was still in the process
	       of being laid out; this occurs when we have recursive
	       class dependencies via inner classes.  We must record
	       the previous alias set and restore it after laying out
	       the class.

	       FIXME: this really is a kludge.  We should figure out a
	       way to lay out the class properly before this
	       happens.  */
	    if (TYPE_SIZE (klass) && CLASSTYPE_SUPER (klass)
		&& integer_zerop (TYPE_SIZE (klass)))
	      {
		TYPE_SIZE (klass) = NULL_TREE;
		previous_alias_set = TYPE_ALIAS_SET (klass);
		TYPE_ALIAS_SET (klass) = -1;
	      }

	    if (! CLASS_LOADED_P (klass))
	      {
		if (! CLASS_PARSED_P (klass))
		  {
		    read_zip_member (current_jcf, zdir, localToFile);
		    jcf_parse (current_jcf);
		  }
		layout_class (current_class);
		load_inner_classes (current_class);
	      }

	    if (previous_alias_set != -1)
	      TYPE_ALIAS_SET (klass) = previous_alias_set;

	    if (TYPE_SIZE (current_class) != error_mark_node)
	      {
		parse_class_file ();
		free (current_jcf->buffer); /* No longer necessary */
		/* Note: there is a way to free this buffer right after a
		   class seen in a zip file has been parsed. The idea is the
		   set its jcf in such a way that buffer will be reallocated
		   the time the code for the class will be generated. FIXME. */
	      }
	  }
	  break;

	case 2:
	  {
	    char *file_name, *class_name_in_zip_dir, *buffer;
	    JCF *jcf;
	    file_name = XNEWVEC (char, zdir->filename_length + 1);
	    class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);
	    strncpy (file_name, class_name_in_zip_dir, zdir->filename_length);
	    file_name[zdir->filename_length] = '\0';
	    jcf = XNEW (JCF);
	    JCF_ZERO (jcf);
	    jcf->read_state  = finput;
	    jcf->filbuf      = jcf_filbuf_from_stdio;
	    jcf->classname   = NULL;
	    jcf->filename    = file_name;
	    jcf->zipd        = zdir;

	    if (read_zip_member (jcf, zdir, localToFile) < 0)
	      fatal_error ("error while reading %s from zip file", file_name);

	    buffer = XNEWVEC (char, zdir->filename_length + 1 +
			    (jcf->buffer_end - jcf->buffer));
	    strcpy (buffer, file_name);
	    /* This is not a typo: we overwrite the trailing \0 of the
	       file name; this is just how the data is laid out.  */
	    memcpy (buffer + zdir->filename_length,
		    jcf->buffer, jcf->buffer_end - jcf->buffer);

	    compile_resource_data (file_name, buffer,
				   jcf->buffer_end - jcf->buffer);
	    JCF_FINISH (jcf);
	    free (jcf);
	    free (buffer);
	  }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Read all the entries of the zip file, creates a class and a JCF. Sets the
   jcf up for further processing and link it to the created class.  */

static void
process_zip_dir (FILE *finput)
{
  int i;
  ZipDirectory *zdir;

  for (i = 0, zdir = (ZipDirectory *)localToFile->central_directory;
       i < localToFile->count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      char *class_name, *file_name, *class_name_in_zip_dir;
      tree klass;
      JCF  *jcf;

      class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

      /* Here we skip non-class files; we handle them later.  */
      if (classify_zip_file (zdir) != 1)
	continue;

      class_name = compute_class_name (zdir);
      file_name  = XNEWVEC (char, zdir->filename_length+1);
      jcf = ggc_alloc_cleared_JCF ();

      strncpy (file_name, class_name_in_zip_dir, zdir->filename_length);
      file_name [zdir->filename_length] = '\0';

      klass = lookup_class (get_identifier (class_name));

      if (CLASS_FROM_CURRENTLY_COMPILED_P (klass))
	{
	  /* We've already compiled this class.  */
	  duplicate_class_warning (file_name);
	  continue;
	}
      /* This function is only called when processing a zip file seen
	 on the command line.  */
      CLASS_FROM_CURRENTLY_COMPILED_P (klass) = 1;

      jcf->read_state  = finput;
      jcf->filbuf      = jcf_filbuf_from_stdio;
      jcf->classname   = class_name;
      jcf->filename    = file_name;
      jcf->zipd        = zdir;

      TYPE_JCF (klass) = jcf;
    }
}

#include "gt-java-jcf-parse.h"
#include "gtype-java.h"
