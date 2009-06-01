/* Utility routines for finding and reading Java(TM) .class files.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
   2006, 2007, 2008, 2009 Free Software Foundation, Inc.

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

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

#include "jcf.h"
#include "tree.h"
#include "toplev.h"
#include "java-tree.h"
#include "hashtab.h"
#include <dirent.h>

#include "zlib.h"

/* DOS brain-damage */
#ifndef O_BINARY
#define O_BINARY 0 /* MS-DOS brain-damage */
#endif

int
jcf_unexpected_eof (JCF *jcf, int count ATTRIBUTE_UNUSED)
{
  if (jcf->filename)
    fprintf (stderr, "Premature end of .class file %s.\n", jcf->filename);
  else
    fprintf (stderr, "Premature end of .class file <stdin>.\n");
  exit (-1);
}

void
jcf_trim_old_input (JCF *jcf)
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
jcf_filbuf_from_stdio (JCF *jcf, int count)
{
  FILE *file = (FILE*) (jcf->read_state);
  if (count > jcf->buffer_end - jcf->read_ptr)
    {
      JCF_u4 old_read_ptr = jcf->read_ptr - jcf->buffer;
      JCF_u4 old_read_end = jcf->read_end - jcf->buffer;
      JCF_u4 old_size = jcf->buffer_end - jcf->buffer;
      JCF_u4 new_size = (old_size == 0 ? 2000 : 2 * old_size) + count;
      unsigned char *new_buffer
	= jcf->buffer == NULL ? XNEWVAR (unsigned char, new_size)
	: XRESIZEVAR (unsigned char, jcf->buffer, new_size);
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

struct ZipFile *SeenZipFiles = NULL;

/* Open a zip file with the given name, and cache directory and file
   descriptor.  If the file is missing, treat it as an empty archive.
   Return NULL if the .zip file is malformed.
*/

ZipFile *
opendir_in_zip (const char *zipfile, int is_system)
{
  struct ZipFile* zipf;
  char magic [4];
  int fd;
  for (zipf = SeenZipFiles;  zipf != NULL;  zipf = zipf->next)
    {
      if (strcmp (zipf->name, zipfile) == 0)
	return zipf;
    }

  zipf = XNEWVAR (struct ZipFile, sizeof (struct ZipFile) + strlen (zipfile) + 1);
  zipf->next = SeenZipFiles;
  zipf->name = (char*)(zipf+1);
  strcpy (zipf->name, zipfile);
  fd = open (zipfile, O_RDONLY | O_BINARY);
  zipf->fd = fd;
  if (fd < 0)
    {
      /* A missing zip file is not considered an error.
       We may want to re-consider that.  FIXME. */
      zipf->count = 0;
      zipf->dir_size = 0;
      zipf->central_directory = NULL;
    }
  else
    {
      jcf_dependency_add_file (zipfile, is_system);
      if (read (fd, magic, 4) != 4 || GET_u4 (magic) != (JCF_u4)ZIPMAGIC)
	{
	  free (zipf);
	  close (fd);
	  return NULL;
	}
      lseek (fd, 0L, SEEK_SET);
      if (read_zip_archive (zipf) != 0)
	{
	  free (zipf);
	  close (fd);
	  return NULL;
	}
    }

  SeenZipFiles = zipf;  
  return zipf;
}

/* Returns:
   0:  OK - zipmember found.
   -1: Not found.
   -2: Malformed archive.
*/

int
open_in_zip (JCF *jcf, const char *zipfile, const char *zipmember,
	     int is_system)
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

	  jcf->filename = xstrdup (zipfile);
	  jcf->classname = xstrdup (zipmember);
	  return read_zip_member(jcf, zipd, zipf);
	}
    }
  return -1;
}

/* Read data from zip archive member. */

int
read_zip_member (JCF *jcf,  ZipDirectory *zipd, ZipFile *zipf)
{
  jcf->filbuf = jcf_unexpected_eof;
  jcf->zipd = zipd;

  if (zipd->compression_method == Z_NO_COMPRESSION)
    {
      jcf->buffer = XNEWVEC (unsigned char, zipd->size);
      jcf->buffer_end = jcf->buffer + zipd->size;
      jcf->read_ptr = jcf->buffer;
      jcf->read_end = jcf->buffer_end;
      if (lseek (zipf->fd, zipd->filestart, 0) < 0
	  || read (zipf->fd, jcf->buffer, zipd->size) != (long) zipd->size)
	return -2;
    }
  else
    {
      char *buffer;
      z_stream d_stream; /* decompression stream */
      d_stream.zalloc = (alloc_func) 0;
      d_stream.zfree = (free_func) 0;
      d_stream.opaque = (voidpf) 0;

      jcf->buffer = XNEWVEC (unsigned char, zipd->uncompressed_size);
      d_stream.next_out = jcf->buffer;
      d_stream.avail_out = zipd->uncompressed_size;
      jcf->buffer_end = jcf->buffer + zipd->uncompressed_size;
      jcf->read_ptr = jcf->buffer;
      jcf->read_end = jcf->buffer_end;
      buffer = XNEWVEC (char, zipd->size);
      d_stream.next_in = (unsigned char *) buffer;
      d_stream.avail_in = zipd->size;
      if (lseek (zipf->fd, zipd->filestart, 0) < 0
	  || read (zipf->fd, buffer, zipd->size) != (long) zipd->size)
	return -2;
      /* Handle NO_HEADER using undocumented zlib feature.
	 This is a very common hack.  */
      inflateInit2 (&d_stream, -MAX_WBITS);
      inflate (&d_stream, Z_NO_FLUSH);
      inflateEnd (&d_stream);
      free (buffer);
    }

  return 0;
}

const char *
open_class (const char *filename, JCF *jcf, int fd, const char *dep_name)
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
      jcf->buffer = XNEWVEC (unsigned char, stat_buf.st_size);
      jcf->buffer_end = jcf->buffer + stat_buf.st_size;
      jcf->read_ptr = jcf->buffer;
      jcf->read_end = jcf->buffer_end;
      jcf->read_state = NULL;
      jcf->filename = xstrdup (filename);
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


const char *
find_classfile (char *filename, JCF *jcf, const char *dep_name)
{
  int fd = open (filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    return NULL;
  return open_class (filename, jcf, fd, dep_name);
}

/* Returns 1 if the CLASSNAME (really a char *) matches the name
   stored in TABLE_ENTRY (also a char *).  */

static int
memoized_class_lookup_eq (const void *table_entry, const void *classname)
{
  return strcmp ((const char *)classname, (const char *)table_entry) == 0;
}

/* A hash table keeping track of class names that were not found
   during class lookup.  (There is no need to cache the values
   associated with names that were found; they are saved in
   IDENTIFIER_CLASS_VALUE.)  */
static htab_t memoized_class_lookups;

/* Returns a freshly malloc'd string with the fully qualified pathname
   of the .class file for the class CLASSNAME.  CLASSNAME must be
   allocated in permanent storage; this function may retain a pointer
   to it.  Returns NULL on failure.  If JCF != NULL, it is suitably
   initialized.  SOURCE_OK is true if we should also look for .java
   file. */

const char *
find_class (const char *classname, int classname_length, JCF *jcf)
{
  int fd;
  int i, k, klass = -1;
  struct stat class_buf;
  char *dep_file;
  void *entry;
  int buflen;
  char *buffer;
  hashval_t hash;

  /* Create the hash table, if it does not already exist.  */
  if (!memoized_class_lookups)
    memoized_class_lookups = htab_create (37, 
					  htab_hash_string, 
					  memoized_class_lookup_eq,
					  NULL);

  /* Loop for this class in the hashtable.  If it is present, we've
     already looked for this class and failed to find it.  */
  hash = htab_hash_string (classname);
  if (htab_find_with_hash (memoized_class_lookups, classname, hash))
    return NULL;

  /* Allocate and zero out the buffer, since we don't explicitly put a
     null pointer when we're copying it below.  */
  buflen = jcf_path_max_len () + classname_length + 10;
  buffer = XNEWVAR (char, buflen);
  memset (buffer, 0, buflen);

  for (entry = jcf_path_start (); entry != NULL; entry = jcf_path_next (entry))
    {
      const char *path_name = jcf_path_name (entry);
      if (klass != 0)
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
	  klass = stat (buffer, &class_buf);
	}
    }

  dep_file = buffer;
  if (!klass)
    {
      SOURCE_FRONTEND_DEBUG ((stderr, "[Class selected: %s]\n",
			      classname+classname_length-
			      (classname_length <= 30 ? 
			       classname_length : 30)));
      fd = JCF_OPEN_EXACT_CASE (buffer, O_RDONLY | O_BINARY);
      if (fd >= 0)
	goto found;
    }

  free (buffer);

  /* Remember that this class could not be found so that we do not
     have to look again.  */
  *htab_find_slot_with_hash (memoized_class_lookups, classname, hash, INSERT)
    = (void *) CONST_CAST (char *, classname);

  return NULL;
 found:
  {
    const char *const tmp = open_class (buffer, jcf, fd, dep_file);
    jcf->classname = xstrdup (classname);
    return tmp;
  }
}

void
jcf_print_char (FILE *stream, int ch)
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
jcf_print_utf8 (FILE *stream, const unsigned char *str, int length)
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
jcf_print_utf8_replace (FILE *stream, const unsigned char *str, int length,
			int in_char, int out_char)
{
  const unsigned char *limit = str + length;
  while (str < limit)
    {
      int ch = UTF8_GET (str, limit);
      if (ch < 0)
	{
	  fprintf (stream, "\\<invalid>");
	  return;
	}
      jcf_print_char (stream, ch == in_char ? out_char : ch);
    }
}

/* Check that all the cross-references in the constant pool are
   valid.  Returns 0 on success.
   Otherwise, returns the index of the (first) invalid entry.
   Only checks internal consistency, but does not check that
   any classes, fields, or methods are valid.*/

int
verify_constant_pool (JCF *jcf)
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
format_uint (char *buffer, uint64 value, int base)
{
#define WRITE_BUF_SIZE (4 + sizeof(uint64) * 8)
  char buf[WRITE_BUF_SIZE];
  char *buf_ptr = buf+WRITE_BUF_SIZE; /* End of buf. */
  int chars_written;
  int i;

  /* Now do the actual conversion, placing the result at the *end* of buf. */
  /* Note this code does not pretend to be optimized. */
  do {
    int digit = value % base;
    static const char digit_chars[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    *--buf_ptr = digit_chars[digit];
    value /= base;
  } while (value != 0);

  chars_written = buf+WRITE_BUF_SIZE - buf_ptr;
  for (i = 0; i < chars_written; i++)
    buffer[i] = *buf_ptr++;
  buffer[i] = 0;
}

void
format_int (char *buffer, jlong value, int base)
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
