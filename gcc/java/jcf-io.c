/* Utility routines for finding and reading Java(TM) .class files.
   Copyright (C) 1996, 97-98, 1999  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#include "config.h"
#include "system.h"

#include "jcf.h"
#include "tree.h"
#include "toplev.h"
#include "java-tree.h"

/* DOS brain-damage */
#ifndef O_BINARY
#define O_BINARY 0 /* MS-DOS brain-damage */
#endif

int
DEFUN(jcf_unexpected_eof, (jcf, count),
      JCF *jcf AND int count ATTRIBUTE_UNUSED)
{
  if (jcf->filename)
    fprintf (stderr, "Premature end of .class file %s.\n", jcf->filename);
  else
    fprintf (stderr, "Premature end of .class file <stdin>.\n");
  exit (-1);
}

void
DEFUN(jcf_trim_old_input, (jcf),
      JCF *jcf)
{
  int count = jcf->read_ptr - jcf->buffer;
  if (count > 0)
    {
      memmove (jcf->buffer, jcf->read_ptr, jcf->read_end - jcf->read_ptr);
      jcf->read_ptr -= count;
      jcf->read_end -= count;
    }
}

int
DEFUN(jcf_filbuf_from_stdio, (jcf, count),
      JCF *jcf AND int count)
{
  FILE *file = (FILE*) (jcf->read_state);
  if (count > jcf->buffer_end - jcf->read_ptr)
    {
      JCF_u4 old_read_ptr = jcf->read_ptr - jcf->buffer;
      JCF_u4 old_read_end = jcf->read_end - jcf->buffer;
      JCF_u4 old_size = jcf->buffer_end - jcf->buffer;
      JCF_u4 new_size = (old_size == 0 ? 2000 : 2 * old_size) + count;
      unsigned char *new_buffer = jcf->buffer == NULL ? ALLOC (new_size)
	: REALLOC (jcf->buffer, new_size);
      jcf->buffer = new_buffer;
      jcf->buffer_end = new_buffer + new_size;
      jcf->read_ptr = new_buffer + old_read_ptr;
      jcf->read_end = new_buffer + old_read_end;
    }
  count -= jcf->read_end - jcf->read_ptr;
  if (count <= 0)
    return 0;
  if ((int) fread (jcf->read_end, 1, count, file) != count)
    jcf_unexpected_eof (jcf, count);
  jcf->read_end += count;
  return 0;
}

#include "zipfile.h"

struct ZipFileCache *SeenZipFiles = NULL;

/* Open a zip file with the given name, and cache directory and file
   descriptor.  If the file is missing, treat it as an empty archive.
   Return NULL if the .zip file is malformed.
*/

ZipFile *
DEFUN(opendir_in_zip, (zipfile, is_system),
      const char *zipfile AND int is_system)
{
  struct ZipFileCache* zipf;
  char magic [4];
  int fd;
  for (zipf = SeenZipFiles;  zipf != NULL;  zipf = zipf->next)
    {
      if (strcmp (zipf->name, zipfile) == 0)
	return &zipf->z;
    }

  zipf = ALLOC (sizeof (struct ZipFileCache) + strlen (zipfile) + 1);
  zipf->next = SeenZipFiles;
  zipf->name = (char*)(zipf+1);
  strcpy (zipf->name, zipfile);
  SeenZipFiles = zipf;
  fd = open (zipfile, O_RDONLY | O_BINARY);
  zipf->z.fd = fd;
  if (fd < 0)
    {
      /* A missing zip file is not considered an error.
       We may want to re-consider that.  FIXME. */
      zipf->z.count = 0;
      zipf->z.dir_size = 0;
      zipf->z.central_directory = NULL;
    }
  else
    {
      jcf_dependency_add_file (zipfile, is_system);
      if (read (fd, magic, 4) != 4 || GET_u4 (magic) != (JCF_u4)ZIPMAGIC)
	return NULL;
      lseek (fd, 0L, SEEK_SET);
      if (read_zip_archive (&zipf->z) != 0)
	return NULL;
    }
  return &zipf->z;
}

/* Returns:
   0:  OK - zipmember found.
   -1: Not found.
   -2: Malformed archive.
*/

int
DEFUN(open_in_zip, (jcf, zipfile, zipmember, is_system),
      JCF *jcf AND const char *zipfile AND const char *zipmember
      AND int is_system)
{
  ZipDirectory *zipd;
  int i, len;
  ZipFile *zipf = opendir_in_zip (zipfile, is_system);

  if (zipf == NULL)
    return -2;

  if (!zipmember)
    return 0;

  len = strlen (zipmember);
  
  zipd = (struct ZipDirectory*) zipf->central_directory;
  for (i = 0; i < zipf->count; i++, zipd = ZIPDIR_NEXT (zipd))
    {
      if (len == zipd->filename_length &&
	  strncmp (ZIPDIR_FILENAME (zipd), zipmember, len) == 0)
	{
	  JCF_ZERO (jcf);
	  jcf->buffer = ALLOC (zipd->size);
	  jcf->buffer_end = jcf->buffer + zipd->size;
	  jcf->read_ptr = jcf->buffer;
	  jcf->read_end = jcf->buffer_end;
	  jcf->filbuf = jcf_unexpected_eof;
	  jcf->filename = xstrdup (zipfile);
	  jcf->classname = xstrdup (zipmember);
	  jcf->zipd = (void *)zipd;
	  if (lseek (zipf->fd, zipd->filestart, 0) < 0
	      || read (zipf->fd, jcf->buffer, zipd->size) != zipd->size)
	    return -2;
	  return 0;
	}
    }
  return -1;
}

#if JCF_USE_STDIO
char*
DEFUN(open_class, (filename, jcf, stream, dep_name),
      char *filename AND JCF *jcf AND FILE* stream AND const char *dep_name)
{
  if (jcf)
    {
      if (dep_name != NULL)
	jcf_dependency_add_file (dep_name, 0);
      JCF_ZERO (jcf);
      jcf->buffer = NULL;
      jcf->buffer_end = NULL;
      jcf->read_ptr = NULL;
      jcf->read_end = NULL;
      jcf->read_state = stream;
      jcf->filbuf = jcf_filbuf_from_stdio;
    }
  else
    fclose (stream);
  return filename;
}
#else
char*
DEFUN(open_class, (filename, jcf, fd, dep_name),
      char *filename AND JCF *jcf AND int fd AND const char *dep_name)
{
  if (jcf)
    {
      struct stat stat_buf;
      if (fstat (fd, &stat_buf) != 0
	  || ! S_ISREG (stat_buf.st_mode))
	{
	  perror ("Could not figure length of .class file");
	  return NULL;
	}
      if (dep_name != NULL)
	jcf_dependency_add_file (dep_name, 0);
      JCF_ZERO (jcf);
      jcf->buffer = ALLOC (stat_buf.st_size);
      jcf->buffer_end = jcf->buffer + stat_buf.st_size;
      jcf->read_ptr = jcf->buffer;
      jcf->read_end = jcf->buffer_end;
      jcf->read_state = NULL;
      jcf->filename = filename;
      if (read (fd, jcf->buffer, stat_buf.st_size) != stat_buf.st_size)
	{
	  perror ("Failed to read .class file");
	  return NULL;
	}
      close (fd);
      jcf->filbuf = jcf_unexpected_eof;
    }
  else
    close (fd);
  return filename;
}
#endif


const char *
DEFUN(find_classfile, (filename, jcf, dep_name),
      char *filename AND JCF *jcf AND const char *dep_name)
{
#if JCF_USE_STDIO
  FILE *stream = fopen (filename, "rb");
  if (stream == NULL)
    return NULL;
  return open_class (arg, jcf, stream, dep_name);
#else
  int fd = open (filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    return NULL;
  return open_class (filename, jcf, fd, dep_name);
#endif
}

/* Returns a freshly malloc'd string with the fully qualified pathname
   of the .class file for the class CLASSNAME.  Returns NULL on
   failure.  If JCF != NULL, it is suitably initialized.
   SOURCE_OK is true if we should also look for .java file. */

const char *
DEFUN(find_class, (classname, classname_length, jcf, source_ok),
      const char *classname AND int classname_length AND JCF *jcf AND int source_ok)

{
#if JCF_USE_STDIO
  FILE *stream;
#else
  int fd;
#endif
  int i, k, java = -1, class = -1;
  struct stat java_buf, class_buf;
  char *dep_file;
  void *entry;
  char *java_buffer;

  /* Allocate and zero out the buffer, since we don't explicitly put a
     null pointer when we're copying it below.  */
  int buflen = jcf_path_max_len () + classname_length + 10;
  char *buffer = (char *) ALLOC (buflen);
  bzero (buffer, buflen);

  java_buffer = (char *) alloca (buflen);

  jcf->java_source = 0;

  for (entry = jcf_path_start (); entry != NULL; entry = jcf_path_next (entry))
    {
      const char *path_name = jcf_path_name (entry);
      if (class != 0)
	{
	  int dir_len;

	  strcpy (buffer, path_name);
	  i = strlen (buffer);

	  /* This is right because we know that `.zip' entries will have a
	     trailing slash.  See jcf-path.c.  */
	  dir_len = i - 1;

	  for (k = 0; k < classname_length; k++, i++)
	    {
	      char ch = classname[k];
	      buffer[i] = ch == '.' ? '/' : ch;
	    }
	  strcpy (buffer+i, ".class");

	  if (jcf_path_is_zipfile (entry))
	    {
	      int err_code;
	      JCF _jcf;
	      buffer[dir_len] = '\0';
	      SOURCE_FRONTEND_DEBUG 
		(("Trying [...%s]:%s", 
		  &buffer[dir_len-(dir_len > 15 ? 15 : dir_len)], 
		  buffer+dir_len+1));
	      if (jcf == NULL)
		jcf = &_jcf;
	      err_code = open_in_zip (jcf, buffer, buffer+dir_len+1,
				      jcf_path_is_system (entry));
	      if (err_code == 0)
		{
		  /* Should we check if .zip is out-of-date wrt .java? */
		  buffer[dir_len] = '(';
		  strcpy (buffer+i, ".class)");
		  if (jcf == &_jcf)
		    JCF_FINISH (jcf);
		  return buffer;
		}
	      else
		continue;
	    }
	  class = stat (buffer, &class_buf);
	}

      if (source_ok)
	{
	  /* Compute name of .java file.  */
	  int l, m;
	  strcpy (java_buffer, path_name);
	  l = strlen (java_buffer);
	  for (m = 0; m < classname_length; ++m)
	    java_buffer[m + l] = (classname[m] == '.' ? '/' : classname[m]);
	  strcpy (java_buffer + m + l, ".java");
	  java = stat (java_buffer, &java_buf);
	  if (java == 0)
	    break;
	}
    }

  /* We preferably pick a class file if we have a chance. If the source
     file is newer than the class file, we issue a warning and parse the
     source file instead.
     There should be a flag to allow people have the class file picked
     up no matter what. FIXME. */
  if (! java && ! class && java_buf.st_mtime >= class_buf.st_mtime)
    {
      char *stripped_class_name = xstrdup (classname);
      int i = strlen (stripped_class_name);
      
      while (stripped_class_name [i] != '.')
	i--;
      
      stripped_class_name [i] = '\0';
      warning ("Source file for class `%s' is newer than its matching class file. Source file used instead", stripped_class_name);
      free (stripped_class_name);
      class = -1;
    }

  if (! java)
    dep_file = java_buffer;
  else
    dep_file = buffer;
#if JCF_USE_STDIO
  if (!class)
    {
      SOURCE_FRONTEND_DEBUG (("Trying %s", buffer));
      stream = fopen (buffer, "rb");
      if (stream)
	goto found;
    }
  /* Give .java a try, if necessary */
  if (!java)
    {
      strcpy (buffer, java_buffer);
      SOURCE_FRONTEND_DEBUG (("Trying %s", buffer));
      stream = fopen (buffer, "r");
      if (stream)
	{
	  jcf->java_source = 1;
	  goto found;
	}
    }
#else
  if (!class)
    {
      SOURCE_FRONTEND_DEBUG ((stderr, "[Class selected: %s]\n",
			      classname+classname_length-
			      (classname_length <= 30 ? 
			       classname_length : 30)));
      fd = open (buffer, O_RDONLY | O_BINARY);
      if (fd >= 0)
	goto found;
    }
  /* Give .java a try, if necessary */
  if (!java)
    {
      strcpy (buffer, java_buffer);
      SOURCE_FRONTEND_DEBUG ((stderr, "[Source selected: %s]\n",
			      classname+classname_length-
			      (classname_length <= 30 ? 
			       classname_length : 30)));
      fd = open (buffer, O_RDONLY);
      if (fd >= 0)
	{
	  jcf->java_source = 1;
	  goto found;
	}
    }
#endif

  free (buffer);
  return NULL;
 found:
#if JCF_USE_STDIO
  if (jcf->java_source)
    return NULL;		/* FIXME */
  else
    return open_class (buffer, jcf, stream, dep_file);
#else
  if (jcf->java_source)
    {
      JCF_ZERO (jcf);		/* JCF_FINISH relies on this */
      jcf->java_source = 1;
      jcf->filename = xstrdup (buffer);
      close (fd);		/* We use STDIO for source file */
    }
  else
    buffer = open_class (buffer, jcf, fd, dep_file);
  jcf->classname = (char *) ALLOC (classname_length + 1);
  strncpy (jcf->classname, classname, classname_length + 1);
  jcf->classname = xstrdup (classname);
  return buffer;
#endif
}

void
DEFUN(jcf_print_char, (stream, ch),
      FILE *stream AND int ch)
{
  switch (ch)
    {
    case '\'':
    case '\\':
    case '\"':
      fprintf (stream, "\\%c", ch);
      break;
    case '\n':
      fprintf (stream, "\\n");
      break;
    case '\t':
      fprintf (stream, "\\t");
      break;
    case '\r':
      fprintf (stream, "\\r");
      break;
    default:
      if (ch >= ' ' && ch < 127)
	putc (ch, stream);
      else if (ch < 256)
	fprintf (stream, "\\%03x", ch);
      else
	fprintf (stream, "\\u%04x", ch);
    }
}

/* Print UTF8 string at STR of length LENGTH bytes to STREAM. */

void
DEFUN(jcf_print_utf8, (stream, str, length),
      FILE *stream AND register const unsigned char *str AND int length)
{
  const unsigned char * limit = str + length;
  while (str < limit)
    {
      int ch = UTF8_GET (str, limit);
      if (ch < 0)
	{
	  fprintf (stream, "\\<invalid>");
	  return;
	}
      jcf_print_char (stream, ch);
    }
}

/* Same as jcf_print_utf8, but print IN_CHAR as OUT_CHAR. */

void
DEFUN(jcf_print_utf8_replace, (stream, str, length, in_char, out_char),
      FILE *stream AND const unsigned char *str AND int length
      AND int in_char AND int out_char)
{

  int i;/* FIXME - actually handle Unicode! */
  for (i = 0; i < length; i++)
    {
      int ch = str[i];
      jcf_print_char (stream, ch == in_char ? out_char : ch);
    }
}

/* Check that all the cross-references in the constant pool are
   valid.  Returns 0 on success.
   Otherwise, returns the index of the (first) invalid entry. */

int
DEFUN(verify_constant_pool, (jcf),
      JCF *jcf)
{
  int i, n;
  for (i = 1; i < JPOOL_SIZE (jcf); i++)
    {
      switch (JPOOL_TAG (jcf, i))
	{
	case CONSTANT_NameAndType:
	  n = JPOOL_USHORT2 (jcf, i);
	  if (n <= 0 || n >= JPOOL_SIZE(jcf)
	      || JPOOL_TAG (jcf, n) != CONSTANT_Utf8)
	    return i;
	  /* ... fall through ... */
	case CONSTANT_Class:
	case CONSTANT_String:
	  n = JPOOL_USHORT1 (jcf, i);
	  if (n <= 0 || n >= JPOOL_SIZE(jcf)
	      || JPOOL_TAG (jcf, n) != CONSTANT_Utf8)
	    return i;
	  break;
	case CONSTANT_Fieldref:
	case CONSTANT_Methodref:
	case CONSTANT_InterfaceMethodref:
	  n = JPOOL_USHORT1 (jcf, i);
	  if (n <= 0 || n >= JPOOL_SIZE(jcf)
	      || JPOOL_TAG (jcf, n) != CONSTANT_Class)
	    return i;
	  n = JPOOL_USHORT2 (jcf, i);
	  if (n <= 0 || n >= JPOOL_SIZE(jcf)
	      || JPOOL_TAG (jcf, n) != CONSTANT_NameAndType)
	    return i;
	  break;
	case CONSTANT_Long:
	case CONSTANT_Double:
	  i++;
	  break;
	case CONSTANT_Float:
	case CONSTANT_Integer:
	case CONSTANT_Utf8:
	case CONSTANT_Unicode:
	  break;
	default:
	  return i;
	}
    }
  return 0;
}

void
DEFUN(format_uint, (buffer, value, base),
      char *buffer AND uint64 value AND int base)
{
#define WRITE_BUF_SIZE (4 + sizeof(uint64) * 8)
  char buf[WRITE_BUF_SIZE];
  register char *buf_ptr = buf+WRITE_BUF_SIZE; /* End of buf. */
  int chars_written;
  int i;

  /* Now do the actual conversion, placing the result at the *end* of buf. */
  /* Note this code does not pretend to be optimized. */
  do {
    int digit = value % base;
    static char digit_chars[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    *--buf_ptr = digit_chars[digit];
    value /= base;
  } while (value != 0);

  chars_written = buf+WRITE_BUF_SIZE - buf_ptr;
  for (i = 0; i < chars_written; i++)
    buffer[i] = *buf_ptr++;
  buffer[i] = 0;
}

void
DEFUN(format_int, (buffer, value, base),
      char *buffer AND jlong value AND int base)
{
  uint64 abs_value;
  if (value < 0)
    {
      abs_value = -(uint64)value;
      *buffer++ = '-';
    }
  else
    abs_value = (uint64) value;
  format_uint (buffer, abs_value, base);
}
