/* Importing Algol 68 module interfaces.
   Copyright (C) 2025 Jose E. Marchesi.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.

   Written by Jose E. Marchesi.

   The following utility functions have been adapted from the Go front-end:

     a68_open_packet
     a68_try_packet_in_directory
     a68_try_suffixes
     a68_find_export_data
     a68_find_object_export_data
     a68_read_export_data

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "tree.h"
#include "target.h"
#include "tm_p.h"
#include "simple-object.h"
#include "varasm.h"
#include "intl.h"
#include "common/common-target.h"
#include "dwarf2asm.h"

#include <string>

#include "a68.h"

/* A few macros to aid parsing of module map strings below.  */

#define SKIP_WHITESPACES(P) while (ISSPACE (*(P))) (P)++

#define PARSE_BASENAME(P,W)						\
  do									\
    {									\
      (W) = (char *) alloca (strlen ((P)));				\
      size_t i = 0;							\
      while ((*(P)) != '=' && !ISSPACE (*(P)) && ((*(P)) != '\0'))	\
	(W)[i++] = *((P)++);						\
      (W)[i] = '\0';							\
    } while (0)

#define PARSE_INDICANT(P,W)						\
  do									\
    {									\
      (W) = (char *) alloca (strlen ((P)));				\
      size_t i = 0;							\
      if (ISALPHA (*(P)))						\
	{								\
	  (W)[i++] = *((P)++);						\
	  while (ISALPHA (*(P)) || ISDIGIT(*(P)) || (*(P)) == '_')	\
	    {								\
	      if ((*(P)) != '_')					\
		(W)[i++] = *((P)++);					\
	    }								\
	}								\
      (W)[i] = '\0';							\
    } while (0)

/* Parse module map information in MAP and add entries to A68_MODULE_FILES
   accordingly.  Existing entries in the map are overriden without warning.

   If MAP is not a valid module map specification then this function returns
   `false' and sets *ERRMSG to some explanatory message.  Otherwise it returns
   `true' and sets *ERRMSG to NULL.  */

bool
a68_process_module_map (const char *map, const char **errmsg)
{
  const char *p = map;

  while (*p != '\0')
    {
      char *filename;
      SKIP_WHITESPACES (p);
      PARSE_BASENAME (p, filename);

      if (p[0] != '=')
	{
	  *errmsg = "expected = after filename";
	  goto error;
	}
      p++;

      /* Parse one or more joined module indicants.  */
      while (p[0] != ':' && p[0] != '\0')
	{
	  char *module;
	  SKIP_WHITESPACES (p);
	  PARSE_INDICANT (p, module);
	  if (module[0] == '\0')
	    {
	      *errmsg = "expected module indicant";
	      goto error;
	    }

	  SKIP_WHITESPACES (p);
	  if (p[0] != ',' && p[0] != ':' && p[0] != '\0')
	    {
	      *errmsg = "expected comma or end of string after module indicant";
	      goto error;
	    }

	  for (char *q = module; *q; ++q)
	    *q = TOUPPER (*q);
	  A68_MODULE_FILES->put (ggc_strdup (module), ggc_strdup (filename));

	  if (p[0] == ',')
	    p++;
	}

      SKIP_WHITESPACES (p);
      if (p[0] != ':' && p[0] != '\0')
	{
	  *errmsg = "expected semicolon or end of string";
	  goto error;
	}

      if (p[0] == ':')
	p++;
    }

  *errmsg = NULL;
  return true;
 error:
  return false;
}

/* Read exports from an object file.

   FD is a file descriptor open for reading.

   OFFSET is the offset within the file where the object file starts; this will
   be 0 except when reading an archive.

   On success this returns NULL and sets *PBUF to a buffer allocated using
   malloc, of size *PLEN, holding the export data.

   If the data is not found this returns NULL and sets *PBUF to NULL and *PLEN
   to 0.

   If some error occurs, this returns an error message and sets *PERR to an
   errno value or 0 if there is no relevant errno.  */

static const char *
a68_read_export_data (int fd, uint64_t offset, char **pbuf, size_t *plen,
		      int *perr)
{
  simple_object_read *sobj;
  const char *errmsg;
  off_t sec_offset;
  off_t sec_length;
  int found;
  char *buf;
  ssize_t c;

  *pbuf = NULL;
  *plen = 0;

  sobj = simple_object_start_read (fd, offset, A68_EXPORT_SEGMENT_NAME,
				   &errmsg, perr);
  if (sobj == NULL)
    {
      /* If we get an error here, just pretend that we didn't find any
	 export data.  This is the right thing to do if the error is
	 that the file was not recognized as an object file.  This
	 will ignore file I/O errors, but it's not too big a deal
	 because we will wind up giving some other error later.  */
      return NULL;
    }

  found = simple_object_find_section (sobj, A68_EXPORT_SECTION_NAME,
				      &sec_offset, &sec_length,
				      &errmsg, perr);
  simple_object_release_read (sobj);
  if (!found)
    return errmsg;

  if (lseek (fd, offset + sec_offset, SEEK_SET) < 0)
    {
      *perr = errno;
      return _("lseek failed while reading export data");
    }

  buf = XNEWVEC (char, sec_length);
  if (buf == NULL)
    {
      *perr = errno;
      return _("memory allocation failed while reading export data");
    }

  c = read (fd, buf, sec_length);
  if (c < 0)
    {
      *perr = errno;
      free (buf);
      return _("read failed while reading export data");
    }

  if (c < sec_length)
    {
      free (buf);
      return _("short read while reading export data");
    }

  *pbuf = buf;
  *plen = sec_length;
  return NULL;
}

/* Look for export data in an object file.  */

static char *
a68_find_object_export_data (const std::string& filename,
			     int fd, uint64_t offset, size_t *psize)
{
  char *buf;
  size_t len;
  int err;

  const char *errmsg = a68_read_export_data (fd, offset, &buf, &len, &err);
  if (errmsg != NULL)
    {
      if (err == 0)
	a68_error (NO_NODE, "Z: Z", filename.c_str (), errmsg);
      else
	a68_error (NO_NODE, "Z: Z: Z", filename.c_str(), errmsg,
		   xstrerror(err));
      return NULL;
    }

  *psize = len;
  return buf;
}

/* Look for export data in the file descriptor FD.  */

static char *
a68_find_export_data (const std::string &filename, int fd, size_t *psize)
{
  /* See if we can read this as an object file.  */
  char *exports = a68_find_object_export_data (filename, fd, 0, psize);
  if (exports != NULL)
    return exports;

  if (lseek (fd, 0, SEEK_SET) < 0)
    {
      a68_error (NO_NODE, "lseek Z failed", filename.c_str ());
      return NULL;
    }

  char buf[A68_EXPORT_MAGIC_LEN];
  ssize_t c = ::read(fd, buf, A68_EXPORT_MAGIC_LEN);
  if (c < A68_EXPORT_MAGIC_LEN)
    return NULL;

  /* Check for a file containing nothing but Algol 68 export data.  */
  if (buf[0] == '\x0a' && buf[1] == '\xad')
    {
      /* XXX read whole file.  */
      return exports;
    }

#if 0
  /* See if we can read this as an archive.  */
  if (Import::is_archive_magic(buf))
    return Import::find_archive_export_data(filename, fd, location);
#endif

  return NULL;

}

/* Given *PFILENAME, where *PFILENAME does not exist, try various suffixes.  If
   we find one, set *FILENAME to the one we found.  Return the open file
   descriptor.  */

static int
a68_try_suffixes (std::string *pfilename)
{
  std::string filename = *pfilename + ".m68";
  int fd = open (filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  const char* basename = lbasename (pfilename->c_str());
  size_t basename_pos = basename - pfilename->c_str ();
  filename = pfilename->substr (0, basename_pos) + "lib" + basename + ".so";
  fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = pfilename->substr (0, basename_pos) + "lib" + basename + ".a";
  fd = open (filename.c_str (), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = *pfilename + ".o";
  fd = open (filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  return -1;
}

/* Try to find the export data for FILENAME.  */

static char *
a68_try_packet_in_directory (const std::string &filename, size_t *psize)
{
  std::string found_filename = filename;
  int fd = a68_try_suffixes (&found_filename);
  if (fd < 0)
    return NULL;

  /* The export data may not be in this file.  */
  char *exports = a68_find_export_data (found_filename, fd, psize);
  if (exports != NULL)
    return exports;

  close (fd);

  a68_error (NO_NODE, "file Z exists but does not contain any export data",
	     found_filename.c_str ());

  return NULL;
}

/* Find import data in FILENAME.

   This searches the file system for FILENAME, reads exports information from
   it, and returns a pointer to the beginning of an allocated buffer with the
   exports data, its size in *psize.  It is up to the caller of this function
   to release the buffer when it is no longer necessary.  If the file is not
   found, this function returns NULL.

   When FILENAME is not an absolute path and does not start with ./ or ../, we
   use the search path provided by -I and -L options.

   When FILENAME does start with ./ or ../, we use RELATIVE_IMPORT_PATH as a
   prefix.

   When FILENAME does not exist, we try
   modifying FILENAME to find the file.
   We use the first of these which exists:

   - We append ".m68".
   - We turn the base of FILENAME info libFILENAME.so.
   - We turn the base of FILENAME into libFILENAME.a.
   - We append ".o".

   When using a search path, we apply each of these transformations at each
   entry on the search path before moving on to the next entry.  If the file
   exists, but does not contain Algol 68 export data, we stop; we do not keep
   looking for another file with the same name later in the search path.  */

static char *
a68_get_packet_exports (const std::string &filename,
			const std::string &relative_import_path,
			size_t *psize)
{
  char *exports;

  bool is_local;
  if (IS_ABSOLUTE_PATH (filename))
    is_local = true;
  else if (filename[0] == '.'
	   && (filename[1] == '\0' || IS_DIR_SEPARATOR (filename[1])))
    is_local = true;
  else if (filename[0] == '.'
	   && filename[1] == '.'
	   && (filename[2] == '\0' || IS_DIR_SEPARATOR (filename[2])))
    is_local = true;
  else
    is_local = false;

  std::string fn = filename;
  if (is_local && !IS_ABSOLUTE_PATH (filename)
      && !relative_import_path.empty ())
    {
      if (fn == ".")
	/* A special case.  */
	fn = relative_import_path;
      else if (fn[0] == '.' && fn[1] == '.'
	       && (fn[2] == '\0' || IS_DIR_SEPARATOR (fn[2])))
	{
	  /* We are going to join relative_import_path and fn, and it will look
	     like DIR/../PATH.  BUt DIR does not necessarily exist in this
	     case, and if it doesn't the use of .. will fail although it
	     shouldn't.  */
	  size_t index;
	  for (index = relative_import_path.length () - 1;
	       index > 0 && !IS_DIR_SEPARATOR (relative_import_path[index]);
	       index--)
	    ;
	  if (index > 0)
	    fn = relative_import_path.substr (0, index) + fn.substr (2);
	  else
	    fn = relative_import_path + '/' + fn;
	}
      else
	fn = relative_import_path + '/' + fn;
      is_local = false;
    }

  if (!is_local)
    {
      for (std::string path : A68_IMPORT_PATHS)
	{
	  if (!path.empty () && path[path.size () - 1] != '/')
	    path += '/';
	  path += fn;
	  exports = a68_try_packet_in_directory (path, psize);
	  if (exports != NULL)
	    return exports;
	}
    }

  return a68_try_packet_in_directory (fn, psize);
}

/* The size of the target's pointer type, in bytes.  */
#ifndef PTR_SIZE
#define PTR_SIZE ((int)(POINTER_SIZE / BITS_PER_UNIT))
#endif

/* Collection of decoding helper macros, to be exclusively used in the
   a68_decode_* functions below.  */

#define DINT8(V)				\
  do						\
    {						\
      if (pos + 1 > size)			\
	goto decode_error;			\
      (V) = (int8_t)data[pos++];		\
    }						\
  while (0)

#define DUINT8(V)				\
  do						\
    {						\
      if (pos + 1 > size)			\
	goto decode_error;			\
      (V) = (uint8_t)data[pos++];		\
    }						\
  while (0)

#define DUINT16(V)							\
  do									\
    {									\
      if (pos + 2 > size)						\
	goto decode_error;						\
      if (BYTES_BIG_ENDIAN)						\
	(V) = ((uint8_t) data[pos] << 8) | (uint8_t) data[pos + 1];	\
      else								\
	(V) = ((uint8_t) data[pos + 1] << 8) | (uint8_t) data[pos];	\
      pos += 2;								\
    }									\
  while (0)

#define DOFFSET(V)							\
  do									\
    {									\
      if (pos + PTR_SIZE > size)					\
	goto decode_error;						\
      (V) = 0;								\
      if (BYTES_BIG_ENDIAN)						\
	{								\
	  for (int i = 0; i < PTR_SIZE;	i++)				\
	    (V) = ((V) | ((uint8_t) data[pos + i] << ((PTR_SIZE - i - 1) * 8))); \
	}								\
      else								\
	{								\
	  for (int i = 0; i < PTR_SIZE;	i++)				\
	    (V) = ((V) | ((uint8_t) data[pos + i] << (i * 8)));		\
	}								\
      pos += PTR_SIZE;							\
    }									\
  while (0)

#define DSTR(V)					\
  do						\
    {						\
      uint16_t len;				\
      char *str = NULL;				\
      DUINT16 (len);				\
      if (pos + len > size)			\
	goto decode_error;			\
      if (len > 0)				\
	{					\
	  str = (char *) xmalloc (len);		\
	  memcpy (str, data + pos, len);	\
	  pos += len;				\
	}					\
      (V) = str;				\
    }						\
  while (0)

/* Types to denote encoded modes.  */

struct encoded_triplet
{
  uint64_t lb;
  uint64_t ub;
};

struct encoded_field
{
  uint64_t mode_offset;
  char *name;
};

struct encoded_arg
{
  uint64_t arg_mode_offset;
  char *arg_name;
};

struct encoded_mode
{
  MOID_T *moid;
  uint64_t offset;
  uint8_t kind;
  int8_t sizety;
  union
  {
    struct
    {
      uint64_t sub_offset;
    } name;

    struct
    {
      uint8_t sub_offset;
    } flex;

    struct
    {
      uint8_t ndims;
      struct encoded_triplet *triplets;
      uint64_t sub_offset;
    } row;

    struct
    {
      uint16_t nfields;
      struct encoded_field *fields;
    } sct;

    struct
    {
      uint8_t nmodes;
      uint64_t *modes;
    } union_;

    struct
    {
      uint64_t ret_mode_offset;
      uint8_t nargs;
      struct encoded_arg *args;
    } proc;

  } data;
};

/* Free the memory used by an encoded mode.  */

static void
encoded_mode_free (struct encoded_mode *em)
{
  switch (em->kind)
    {
    case GA68_MODE_ROW:
      free (em->data.row.triplets);
      break;
    case GA68_MODE_STRUCT:
      /* Note that the field names are installed in moids in
	 encoded_mode_to_moid, so we shoud not free them.  */
      free (em->data.sct.fields);
      break;
    case GA68_MODE_UNION:
      free (em->data.union_.modes);
      break;
    case GA68_MODE_PROC:
      for (uint8_t i = 0; i < em->data.proc.nargs; i++)
	free (em->data.proc.args[i].arg_name);
      free (em->data.proc.args);
      break;
    default:
      break;
    }
  free (em);
}

/* A collection of encoded modes indexed by offsets.  */

#define NO_OFFSET ((uint64_t) -1)

typedef hash_map<int_hash<uint64_t,NO_OFFSET>,
		 struct encoded_mode *> encoded_modes_map_t;

/* Complete a encoded mode.  */

static MOID_T *
complete_encoded_mode (encoded_modes_map_t &encoded_modes, uint64_t offset)
{
  struct encoded_mode *em = *(encoded_modes.get (offset));
  MOID_T *sub;
  PACK_T *pack;

  if (em->moid != NO_MOID)
    return em->moid;

  switch (em->kind)
    {
    case GA68_MODE_VOID:   em->moid = M_VOID; break;
    case GA68_MODE_CHAR:   em->moid = M_CHAR; break;
    case GA68_MODE_BOOL:   em->moid = M_BOOL; break;
    case GA68_MODE_STRING: em->moid = M_FLEX_ROW_CHAR; break;
    case GA68_MODE_INT:
      switch (em->sizety)
	{
	case 0: em->moid = M_INT; break;
	case 1: em->moid = M_LONG_INT; break;
	case 2: em->moid = M_LONG_LONG_INT; break;
	case -1: em->moid = M_SHORT_INT; break;
	case -2: em->moid = M_SHORT_SHORT_INT; break;
	default:
	  gcc_unreachable ();
	}
      break;
    case GA68_MODE_BITS:
      switch (em->sizety)
	{
	case 0: em->moid = M_BITS; break;
	case 1: em->moid = M_LONG_BITS; break;
	case 2: em->moid = M_LONG_LONG_BITS; break;
	case -1: em->moid = M_SHORT_BITS; break;
	case -2: em->moid = M_SHORT_SHORT_BITS; break;
	default:
	  gcc_unreachable ();
	}
      break;
    case GA68_MODE_BYTES:
      switch (em->sizety)
	{
	case 0: em->moid = M_BYTES; break;
	case 1: em->moid = M_LONG_BYTES; break;
	default:
	  gcc_unreachable ();
	}
      break;
    case GA68_MODE_REAL:
      switch (em->sizety)
	{
	case 0: em->moid = M_REAL; break;
	case 1: em->moid = M_LONG_REAL; break;
	case 2: em->moid = M_LONG_LONG_REAL; break;
	default:
	  gcc_unreachable ();
	}
      break;
    case GA68_MODE_CMPL:
      switch (em->sizety)
	{
	case 0: em->moid = M_COMPLEX; break;
	case 1: em->moid = M_LONG_COMPLEX; break;
	case 2: em->moid = M_LONG_LONG_COMPLEX; break;
	default:
	  gcc_unreachable ();
	}
      break;
    case GA68_MODE_NAME:
    case GA68_MODE_FLEX:
      /* For recursive declarations.  */
      em->moid = a68_create_mode (em->kind == GA68_MODE_NAME ? REF_SYMBOL : FLEX_SYMBOL,
				  0, NO_NODE, M_ERROR, NO_PACK);
      sub = complete_encoded_mode (encoded_modes,
				   em->kind == GA68_MODE_NAME
				   ? em->data.name.sub_offset : em->data.flex.sub_offset);
      if (sub == NO_MOID)
	{
	  /* Free em->moid */
	  return NO_MOID;
	}
      SUB (em->moid) = sub;
      break;
    case GA68_MODE_ROW:
      /* XXX how to convey actual bounds.  */
      /* For recursive declarations.  */
      em->moid = a68_create_mode (ROW_SYMBOL, 0, NO_NODE, M_ERROR, NO_PACK);
      sub = complete_encoded_mode (encoded_modes, em->data.row.sub_offset);
      if (sub == NO_MOID)
	{
	  /* Free em->moid */
	  return  NO_MOID;
	}
      SUB (em->moid) = sub;
      DIM (em->moid) = em->data.row.ndims;
      break;
    case GA68_MODE_STRUCT:
      /* For recursive declarations.  */
      em->moid = a68_create_mode (STRUCT_SYMBOL, 0, NO_NODE, NO_MOID, NO_PACK);
      pack = NO_PACK;
      for (uint16_t i = 0; i < em->data.sct.nfields; i++)
	{
	  /* Note we have to do this from last field to first field, because
	     a68_add_mode_to_pack prepends to the list.  */
	  uint16_t index = em->data.sct.nfields - 1 - i;
	  char *field_name = em->data.sct.fields[index].name;
	  MOID_T *field_moid = complete_encoded_mode (encoded_modes,
						      em->data.sct.fields[index].mode_offset);
	  if (field_moid == NO_MOID)
	    {
	      /* XXX free em->moid */
	      em->moid = NO_MOID;
	      return  NO_MOID;
	    }
	  (void) a68_add_mode_to_pack (&pack, field_moid, field_name, NO_NODE);
	}
      DIM (em->moid) = a68_count_pack_members (pack);
      PACK (em->moid) = pack;
      break;
    case GA68_MODE_UNION:
      /* For recursive declarations.  */
      em->moid = a68_create_mode (UNION_SYMBOL, 0, NO_NODE, NO_MOID, NO_PACK);
      pack = NO_PACK;
      for (uint8_t i = 0; i < em->data.union_.nmodes; i++)
	{
	  /* Union alternatives are internally stored in reverse order in the
	     pack.  */
	  uint16_t index = i;
	  MOID_T *united_moid = complete_encoded_mode (encoded_modes,
						       em->data.union_.modes[index]);
	  if (united_moid == NO_MOID)
	    {
	      /* XXX free em->moid */
	      em->moid = NO_MOID;
	      return NO_MOID;
	    }
	  (void) a68_add_mode_to_pack (&pack, united_moid, NO_TEXT, NO_NODE);
	}
      DIM (em->moid) = a68_count_pack_members (pack);
      PACK (em->moid) = pack;
      break;
    case GA68_MODE_PROC:
      /* For recursive declarations.  */
      em->moid = a68_create_mode (PROC_SYMBOL, 0, NO_NODE, NO_MOID, NO_PACK);
      pack = NO_PACK;
      for (uint8_t i = 0; i < em->data.proc.nargs; i++)
	{
	  /* Note we have to do this from last argument mode to first argument
	     mode, because a68_add_mode_to_pack prepends to the list.  */
	  uint16_t index = em->data.proc.nargs - 1 - i;
	  char *arg_name = em->data.proc.args[index].arg_name;
	  MOID_T *arg_moid = complete_encoded_mode (encoded_modes,
						    em->data.proc.args[index].arg_mode_offset);
	  if (arg_moid == NO_MOID)
	    {
	      /* XXX free em->moid */
	      em->moid = NO_MOID;
	      return NO_MOID;
	    }
	  (void) a68_add_mode_to_pack (&pack, arg_moid, arg_name, NO_NODE);
	}
      SUB (em->moid) = complete_encoded_mode (encoded_modes,
					      em->data.proc.ret_mode_offset);
      if (SUB (em->moid) == NO_MOID)
	{
	  /* Free em->moid */
	  em->moid = NO_MOID;
	  return NO_MOID;
	}
      DIM (em->moid) = a68_count_pack_members (pack);
      PACK (em->moid) = pack;
      break;
    default:
      gcc_unreachable ();
    }

  return em->moid;
}

/* Dump the contents of an encoded_mode, for debugging purposes.  */

ATTRIBUTE_UNUSED static void
dump_encoded_mode (struct encoded_mode *em)
{
  printf ("[%" PRIu64 "] kind: %" PRIu8, em->offset, em->kind);
  switch (em->kind)
    {
    case GA68_MODE_VOID:
      printf (" void\n");
      break;
    case GA68_MODE_CHAR:
      printf (" char\n");
      break;
    case GA68_MODE_BOOL:
      printf (" bool\n");
      break;
    case GA68_MODE_STRING:
      printf (" string\n");
      break;
    case GA68_MODE_NAME:
      printf (" name\n");
      printf ("  sub: %" PRIu64 "\n", em->data.name.sub_offset);
      break;
    case GA68_MODE_STRUCT:
      printf (" struct\n");
      printf ("  nfields: %" PRIu16 "\n", em->data.sct.nfields);
      for (uint16_t i = 0; i < em->data.sct.nfields; i++)
	printf ("  %s : [%" PRIu64 "]\n",
		em->data.sct.fields[i].name, em->data.sct.fields[i].mode_offset);
      break;
    case GA68_MODE_FLEX:
      printf (" flex\n");
      printf ("  sub: %" PRIu64 "\n", em->data.name.sub_offset);
      break;
    case GA68_MODE_UNION:
      printf (" union\n");
      printf ("  nmodes: %" PRIu8 "\n", em->data.union_.nmodes);
      printf (" ");
      for (uint8_t i = 0; i < em->data.union_.nmodes; i++)
	printf (" [%" PRIu64 "]", em->data.union_.modes[i]);
      printf ("\n");
      break;
    case GA68_MODE_PROC:
      printf (" proc\n");
      printf ("  retmode: [%" PRIu64 "]\n", em->data.proc.ret_mode_offset);
      printf ("  nargs: %" PRIu8 "\n", em->data.proc.nargs);
      for (uint8_t i = 0; i < em->data.proc.nargs; i++)
	printf ("  %s : [%" PRIu64 "]\n",
		em->data.proc.args[i].arg_name,
		em->data.proc.args[i].arg_mode_offset);
      break;
    case GA68_MODE_ROW:
      printf (" row\n");
      printf ("  ndims: %" PRIu8 "\n", em->data.row.ndims);
      for (uint8_t i = 0; i < em->data.row.ndims; i++)
	{
	  printf ("    lb: %" PRIu64 "\n", em->data.row.triplets[i].lb);
	  printf ("    ub: %" PRIu64 "\n", em->data.row.triplets[i].ub);
	}
      printf ("  sub: [%" PRIu64 "]\n", em->data.row.sub_offset);
      break;
    default:
      break;
    }
}

/* Substitute any reference to mode M in T to R.  */

static void
a68_replace_submode (MOID_T *t, MOID_T *m, MOID_T *r)
{
  if (SUB (t) == m)
    SUB (t) = r;

  for (PACK_T *p = PACK (t); p != NO_PACK; FORWARD (p))
    {
      if (MOID (p) == m)
	MOID (p) = r;
    }
}

/* Substitute mode M with mode R in all modes in MODES_LIST.
   The entry for M in MODES_LIST is set to NO_MOID.  */

static void
a68_replace_equivalent_mode (vec<MOID_T*,va_gc> *mode_list,
			     MOID_T *m, MOID_T *r)
{
  for (size_t i = 0; i < mode_list->length (); ++i)
    {
      if ((*mode_list)[i] == m)
	(*mode_list)[i] = NO_MOID;
      else if ((*mode_list)[i] != NO_MOID)
	a68_replace_submode ((*mode_list)[i], m, r);
    }
}

/* Decode a modes table at DATA + POS.  */

static bool
a68_decode_modes (MOIF_T *moif, encoded_modes_map_t &encoded_modes,
		  const char *data, size_t size, size_t pos,
		  size_t *ppos, const char **errstr)
{
  bool siga;
  uint8_t kind;
  uint64_t mode_table_size, mode_table_end;

  /* Get the size of the modes table.  */
  DOFFSET (mode_table_size);
  mode_table_end = pos + mode_table_size;

  /* Decode all the mode entries and fill in encoded_modes.  */
  while (pos < mode_table_end)
    {
      int8_t sizety;
      uint8_t ndims, nmodes, nargs;
      uint16_t nfields;
      uint64_t mode_offset = pos;
      uint64_t sub, ret_mode_offset;
      struct encoded_mode *encoded_mode;

      DUINT8 (kind);
      encoded_mode = (struct encoded_mode *) xmalloc (sizeof (struct encoded_mode));
      encoded_mode->moid = NO_MOID;
      encoded_mode->offset = mode_offset;
      encoded_mode->kind = kind;
      encoded_mode->sizety = 0;
      switch (kind)
	{
	case GA68_MODE_VOID:
	case GA68_MODE_CHAR:
	case GA68_MODE_BOOL:
	case GA68_MODE_STRING:
	  break;
	case GA68_MODE_INT:
	case GA68_MODE_REAL:
	case GA68_MODE_BITS:
	case GA68_MODE_BYTES:
	case GA68_MODE_CMPL:
	  DINT8 (sizety);
	  encoded_mode->sizety = sizety;
	  break;
	case GA68_MODE_NAME:
	  DOFFSET (sub);
	  encoded_mode->data.name.sub_offset = sub;
	  break;
	case GA68_MODE_FLEX:
	  DOFFSET (sub);
	  encoded_mode->data.flex.sub_offset = sub;
	  break;
	case GA68_MODE_ROW:
	  DUINT8 (ndims);
	  encoded_mode->data.row.triplets
	    = (struct encoded_triplet *) xmalloc (sizeof (struct encoded_triplet) * ndims);
	  for (uint8_t i = 0; i < ndims; i++)
	    {
	      uint64_t lb, ub;
	      DOFFSET (lb);
	      DOFFSET (ub);
	      encoded_mode->data.row.triplets[i].lb = lb;
	      encoded_mode->data.row.triplets[i].ub = ub;
	    }
	  DOFFSET (sub);
	  encoded_mode->data.row.ndims = ndims;
	  encoded_mode->data.row.sub_offset = sub;
	  break;
	case GA68_MODE_UNION:
	  DUINT16 (nmodes);
	  encoded_mode->data.union_.nmodes = nmodes;
	  encoded_mode->data.union_.modes
	    = (uint64_t *) xmalloc (sizeof (uint64_t) * nmodes);
	  for (uint8_t i = 0; i < nmodes; i++)
	    {
	      uint64_t mode;
	      DOFFSET (mode);
	      encoded_mode->data.union_.modes[i] = mode;
	    }
	  break;
	case GA68_MODE_PROC:
	  DOFFSET (ret_mode_offset);
	  DUINT8 (nargs);
	  encoded_mode->data.proc.ret_mode_offset = ret_mode_offset;
	  encoded_mode->data.proc.nargs = nargs;
	  encoded_mode->data.proc.args
	    = (struct encoded_arg *) xmalloc (sizeof (struct encoded_arg) * nargs);
	  for (uint8_t i = 0; i < nargs; i++)
	    {
	      uint64_t arg_mode_offset;
	      char *arg_name;
	      DOFFSET (arg_mode_offset);
	      DSTR (arg_name);
	      encoded_mode->data.proc.args[i].arg_mode_offset = arg_mode_offset;
	      encoded_mode->data.proc.args[i].arg_name = arg_name;
	    }
	  break;
	case GA68_MODE_STRUCT:
	  DUINT16 (nfields);
	  encoded_mode->data.sct.nfields = nfields;
	  encoded_mode->data.sct.fields
	    = (struct encoded_field *) xmalloc (sizeof (struct encoded_field) * nfields);
	  for (uint16_t i = 0; i < nfields; i++)
	    {
	      uint64_t mode_offset;
	      char *field_name;
	      DOFFSET (mode_offset);
	      DSTR (field_name);
	      encoded_mode->data.sct.fields[i].mode_offset = mode_offset;
	      encoded_mode->data.sct.fields[i].name = field_name;
	    }
	  break;
	case GA68_MODE_UNKNOWN:
	default:
	  *errstr = "invalid kind in mode";
	  goto decode_error;
	  break;
	}

      encoded_modes.put (mode_offset, encoded_mode);
    }

  /* Sanity check.  */
  if (pos != mode_table_end)
    {
      *errstr = "invalid mode table size";
      goto decode_error;
    }

  /* Complete all encoded modes.
     This operation must conform a transitive closure.  */
  siga = true;
  while (siga)
    {
      siga = false;
      for (auto entry : encoded_modes)
	{
	  uint64_t offset = entry.first;
	  struct encoded_mode *em = entry.second;

	  if (em->moid == NO_MOID
	      && complete_encoded_mode (encoded_modes, offset) != NO_MOID)
	    siga = true;
	}
    }

  /* At this point all the encoded modes are complete and they are all
     associated with moids.  Put them in the moif.  */
  for (auto entry : encoded_modes)
    {
      struct encoded_mode *em = entry.second;
      vec_safe_push (MODES (moif), em->moid);
    }

  /* Next step is to see if equivalent modes the any of the modes in the moif
      DIM already exist in the compiler's mode list.  In that case, replace the
      DIM moif's mode with the existing mode anywhere in the moif.  */
  for (MOID_T *m : MODES (moif))
    {
      MOID_T *r = a68_search_equivalent_mode (m);
      if (r != NO_MOID)
	{
	  a68_replace_equivalent_mode (MODES (moif), m, r);

	  /* Update encoded_modes to reflect the replacement.  */
	  for (auto entry : encoded_modes)
	    {
	      struct encoded_mode *em = entry.second;
	      if (em->moid == m)
		em->moid = r;
	    }
	}
    }

  *errstr = NULL;
  *ppos = pos;
  return true;
 decode_error:
  if (*errstr == NULL)
    *errstr = "error decoding mode";
  return false;
}

/* Decode an extracts table at DATA + POS.  */

static bool
a68_decode_extracts (MOIF_T *moif, encoded_modes_map_t &encoded_modes,
		     const char *data, size_t size, size_t pos,
		     size_t *ppos, const char **errstr)
{
  uint64_t extracts_table_size, extracts_table_end;

  /* Get the size of the extracts table.  */
  DOFFSET (extracts_table_size);
  extracts_table_end = pos + extracts_table_size;

  /* Decode all the extracts entries, adding them to MOIF as we go.  */
  while (pos < extracts_table_end)
    {
      uint8_t marker, prio, variable, in_proc;
      uint64_t extract_size, extract_end, mode_offset;
      uint64_t mdextra_size;
      char *name;

      EXTRACT_T *e = (EXTRACT_T *) ggc_cleared_alloc<EXTRACT_T> ();

      DOFFSET (extract_size);
      extract_end = pos + extract_size;

      DUINT8 (marker);
      switch (marker)
	{
	case GA68_EXTRACT_MODU:
	  DSTR (name);
	  DOFFSET (mdextra_size);
	  if (mdextra_size != 0)
	    {
	      *errstr = "non-empty mdextra in module extract";
	      goto decode_error;
	    }
	  EXTRACT_KIND (e) = GA68_EXTRACT_MODU;
	  EXTRACT_SYMBOL (e) = ggc_strdup (name);
	  EXTRACT_MODE (e) = NO_MOID;
	  EXTRACT_PRIO (e) = 0;
	  EXTRACT_VARIABLE (e) = false;
	  EXTRACT_IN_PROC (e) = false;
	  vec_safe_push (MODULES (moif), e);
	  break;
	case GA68_EXTRACT_IDEN:
	  DSTR (name);
	  DOFFSET (mode_offset);
	  DOFFSET (mdextra_size);
	  if (mdextra_size != 2)
	    {
	      *errstr = "mdextra size should be 2 in iden extract";
	      goto decode_error;
	    }
	  DUINT8 (variable);
	  DUINT8 (in_proc);
	  EXTRACT_KIND (e) = GA68_EXTRACT_IDEN;
	  EXTRACT_SYMBOL (e) = ggc_strdup (name);
	  EXTRACT_MODE (e) = (*(encoded_modes.get (mode_offset)))->moid;
	  EXTRACT_PRIO (e) = 0;
	  EXTRACT_VARIABLE (e) = variable;
	  EXTRACT_IN_PROC (e) = in_proc;
	  vec_safe_push (IDENTIFIERS (moif), e);
	  break;
	case GA68_EXTRACT_MODE:
	  DSTR (name);
	  DOFFSET (mode_offset);
	  DOFFSET (mdextra_size);
	  if (mdextra_size != 0)
	    {
	      *errstr = "non-empty mdextra in indicant extract";
	      goto decode_error;
	    }
	  EXTRACT_KIND (e) = GA68_EXTRACT_MODE;
	  EXTRACT_SYMBOL (e) = ggc_strdup (name);
	  EXTRACT_MODE (e) = (*(encoded_modes.get (mode_offset)))->moid;
	  EXTRACT_PRIO (e) = 0;
	  EXTRACT_VARIABLE (e) = false;
	  EXTRACT_IN_PROC (e) = false;
	  vec_safe_push (INDICANTS (moif), e);
	  break;
	case GA68_EXTRACT_PRIO:
	  DSTR (name);
	  DUINT8 (prio);
	  DOFFSET (mdextra_size);
	  if (mdextra_size != 0)
	    {
	      *errstr = "non-empty mdextra in prio extract";
	      goto decode_error;
	    }
	  EXTRACT_KIND (e) = GA68_EXTRACT_PRIO;
	  EXTRACT_SYMBOL (e) = ggc_strdup (name);
	  EXTRACT_MODE (e) = NO_MOID;
	  EXTRACT_PRIO (e) = prio;
	  EXTRACT_VARIABLE (e) = false;
	  EXTRACT_IN_PROC (e) = false;
	  vec_safe_push (PRIOS (moif), e);
	  break;
	case GA68_EXTRACT_OPER:
	  DSTR (name);
	  DOFFSET (mode_offset);
	  DOFFSET (mdextra_size);
	  if (mdextra_size != 2)
	    {
	      *errstr = "mdextra size should be 2 in oper extract";
	      goto decode_error;
	    }
	  DUINT8 (variable);
	  DUINT8 (in_proc);
	  EXTRACT_KIND (e) = GA68_EXTRACT_OPER;
	  EXTRACT_SYMBOL (e) = ggc_strdup (name);
	  EXTRACT_MODE (e) = (*(encoded_modes.get (mode_offset)))->moid;
	  EXTRACT_PRIO (e) = 0;
	  EXTRACT_VARIABLE (e) = variable;
	  EXTRACT_IN_PROC (e) = in_proc;
	  vec_safe_push (OPERATORS (moif), e);
	  break;
	default:
	  *errstr = "invalid marker in extract";
	  goto decode_error;
	  break;
	}

      /* Sanity check.  */
      if (pos != extract_end)
	{
	  *errstr = "invalid extract size";
	  goto decode_error;
	}
    }

  /* Sanity check.  */
  if (pos != extracts_table_end)
    {
      *errstr = "invalid extracts table size";
      goto decode_error;
    }

  *errstr = NULL;
  *ppos = pos;
  return true;
 decode_error:
  if (*errstr == NULL)
    *errstr = "error decoding extract";
  return false;
}

/* Decode the given exports data into a moif.  If there is a decoding error
   then put an explicative mssage in *ERRSTR and return NULL.  */

static MOIF_T *
a68_decode_moif (const char *data, size_t size, const char **errstr)
{
  size_t pos = 0;
  MOIF_T *moif = a68_moif_new (NULL /* name */);
  encoded_modes_map_t encoded_modes (16);

  uint8_t magic1, magic2;
  uint16_t version;
  char *name, *prelude, *postlude;

  DUINT8 (magic1);
  DUINT8 (magic2);
  if (magic1 != A68_EXPORT_MAGIC1 || magic2 != A68_EXPORT_MAGIC2)
    {
      *errstr = "invalid magic number";
      goto decode_error;
    }

  DUINT16 (version);
  if (version != 1)
    {
      *errstr = "invalid a68 exports version";
      goto decode_error;
    }

  DSTR (name);
  DSTR (prelude);
  DSTR (postlude);
  NAME (moif) = name;
  PRELUDE (moif) = prelude;
  POSTLUDE (moif) = postlude;

  /* Decode the modes table.
     This installs the resulting moids in MOIF.  */
  if (!a68_decode_modes (moif, encoded_modes, data, size, pos, &pos, errstr))
    goto decode_error;

  /* Decode the extracts table.
     This installs the resulting tags in MOIF.  */
  if (!a68_decode_extracts (moif, encoded_modes, data, size, pos, &pos, errstr))
    goto decode_error;

  /* We don't need the encoded modes anymore.  */
  for (auto entry : encoded_modes)
    {
      struct encoded_mode *em = entry.second;
      encoded_mode_free (em);
    }

  /* Got some juicy exports for youuuuuu... */
  return moif;
 decode_error:
  if (*errstr == NULL)
    *errstr = "premature end of data";
  return NULL;
}

/* Get a moif with the exports for module named MODULE.  If no exports can be
   found then return NULL.  */

MOIF_T *
a68_open_packet (const char *module)
{
  /* Look in the modules location maps to see if there is an entry for MODULE.
     If there is one, use the specified filename.  Otherwise canonicalize the
     module name to a file name.  */
  char *filename;
  const char **pfilename = A68_MODULE_FILES->get (module);
  if (pfilename == NULL)
    {
      /* Turn the module indicant in MODULE to lower-case.  */
      filename = (char *) alloca (strlen (module) + 1);
      size_t i = 0;
      for (; i < strlen (module); i++)
	filename[i] = TOLOWER (module[i]);
      filename[i] = '\0';
    }
  else
    {
      size_t len = strlen (*pfilename) + 1;
      filename = (char *) alloca (len);
      memcpy (filename, *pfilename, len);
    }

  /* Try to read exports data in a buffer.  */
  char *exports_data;
  size_t exports_data_size;
  exports_data = a68_get_packet_exports (std::string (filename),
					 std::string ("."),
					 &exports_data_size);
  if (exports_data == NULL)
    return NULL;

  /* Got some data.  Parse it into a moif.  */
  const char *errstr = NULL;
  MOIF_T *moif = a68_decode_moif (exports_data, exports_data_size, &errstr);
  return moif;
}
