/* File format for coverage information
   Copyright (C) 1996-2024 Free Software Foundation, Inc.
   Contributed by Bob Manson <manson@cygnus.com>.
   Completely remangled by Nathan Sidwell <nathan@codesourcery.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Routines declared in gcov-io.h.  This file should be #included by
   another source file, after having #included gcov-io.h.  */

static gcov_unsigned_t *gcov_read_words (void *buffer, unsigned);

/* Indicates the last gcov file access error or that no error occurred
   so far.  */
enum gcov_file_error
{
  GCOV_FILE_COUNTER_OVERFLOW = -1,
  GCOV_FILE_NO_ERROR = 0,
  GCOV_FILE_WRITE_ERROR = 1,
  GCOV_FILE_EOF = 2
};

struct gcov_var
{
  FILE *file;
  enum gcov_file_error error;
  int mode;			/* < 0 writing, > 0 reading.  */
  int endian;			/* Swap endianness.  */
#ifdef IN_GCOV_TOOL
  gcov_position_t pos;		/* File position for stdin support.  */
#endif
} gcov_var;

#define GCOV_MODE_STDIN 2

/* Save the current position in the gcov file.  */
/* We need to expose this function when compiling for gcov-tool.  */
#ifndef IN_GCOV_TOOL
static inline
#endif
gcov_position_t
gcov_position (void)
{
#ifdef IN_GCOV_TOOL
  if (gcov_var.mode == GCOV_MODE_STDIN)
    return gcov_var.pos;
#endif
  return ftell (gcov_var.file);
}

/* Return nonzero if the error flag is set.  */
/* We need to expose this function when compiling for gcov-tool.  */
#ifndef IN_GCOV_TOOL
static inline
#endif
int
gcov_is_error (void)
{
  return gcov_var.file ? gcov_var.error : 1;
}

#if IN_LIBGCOV
/* Move to beginning of file, initialize for writing, and clear file error
   status.  */

GCOV_LINKAGE inline void
gcov_rewrite (void)
{
  gcov_var.mode = -1; 
  gcov_var.error = GCOV_FILE_NO_ERROR;
  fseek (gcov_var.file, 0L, SEEK_SET);
}
#endif

static inline gcov_unsigned_t
from_file (gcov_unsigned_t value)
{
#if !IN_LIBGCOV || defined (IN_GCOV_TOOL)
  if (gcov_var.endian)
    return __builtin_bswap32 (value);
#endif
  return value;
}

/* Open a gcov file. NAME is the name of the file to open and MODE
   indicates whether a new file should be created, or an existing file
   opened. If MODE is >= 0 an existing file will be opened, if
   possible, and if MODE is <= 0, a new file will be created. Use
   MODE=0 to attempt to reopen an existing file and then fall back on
   creating a new one.  If MODE > 0, the file will be opened in
   read-only mode.  Otherwise it will be opened for modification.
   Return zero on failure, non-zero on success.  */

GCOV_LINKAGE int
gcov_open (const char *name, int mode)
{
#if GCOV_LOCKED
  struct flock s_flock;
  int fd;

  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0; /* Until EOF.  */
  s_flock.l_pid = getpid ();
#elif GCOV_LOCKED_WITH_LOCKING
  int fd;
#endif

  gcov_nonruntime_assert (!gcov_var.file);
  gcov_var.error = GCOV_FILE_NO_ERROR;
#if !IN_LIBGCOV || defined (IN_GCOV_TOOL)
  gcov_var.endian = 0;
#endif
#ifdef IN_GCOV_TOOL
  gcov_var.pos = 0;
  if (!name)
    {
      gcov_nonruntime_assert (gcov_var.mode > 0);
      gcov_var.file = stdin;
      gcov_var.mode = GCOV_MODE_STDIN;
      return 1;
    }
#endif
#if GCOV_LOCKED
  if (mode > 0)
    {
      /* Read-only mode - acquire a read-lock.  */
      s_flock.l_type = F_RDLCK;
      /* pass mode (ignored) for compatibility */
      fd = open (name, O_RDONLY, S_IRUSR | S_IWUSR);
    }
  else
     {
       /* Write mode - acquire a write-lock.  */
       s_flock.l_type = F_WRLCK;
       /* Truncate if force new mode.  */
       fd = open (name, O_RDWR | O_CREAT | (mode < 0 ? O_TRUNC : 0), 0666);
    }
  if (fd < 0)
    return 0;

  while (fcntl (fd, F_SETLKW, &s_flock) && errno == EINTR)
    continue;

  gcov_var.file = fdopen (fd, (mode > 0) ? "rb" : "r+b");

  if (!gcov_var.file)
    {
      close (fd);
      return 0;
    }
#elif GCOV_LOCKED_WITH_LOCKING
  if (mode > 0)
    {
      /* pass mode (ignored) for compatibility */
      fd = open (name, O_RDONLY | O_BINARY, S_IRUSR | S_IWUSR);
    }
  else
     {
       /* Truncate if force new mode.  */
       fd = open (name, O_RDWR | O_BINARY | O_CREAT | (mode < 0 ? O_TRUNC : 0),
		  0666);
    }
  if (fd < 0)
    return 0;

  if (_locking (fd, _LK_LOCK, LONG_MAX) < 0)
    {
      close (fd);
      return 0;
    }

  gcov_var.file = fdopen (fd, (mode > 0) ? "rb" : "r+b");

  if (!gcov_var.file)
    {
      close (fd);
      return 0;
    }
#else
  if (mode >= 0)
    /* Open an existing file.  */
    gcov_var.file = fopen (name, (mode > 0) ? "rb" : "r+b");

  if (gcov_var.file)
    mode = 1;
  else if (mode <= 0)
    /* Create a new file.  */
    gcov_var.file = fopen (name, "w+b");

  if (!gcov_var.file)
    return 0;
#endif

  gcov_var.mode = mode ? mode : 1;

  return 1;
}

/* Close the current gcov file. Flushes data to disk. Returns nonzero
   on failure or error flag set.  */

GCOV_LINKAGE int
gcov_close (void)
{
#ifdef IN_GCOV_TOOL
  if (gcov_var.file == stdin)
    gcov_var.file = 0;
  else
#endif
  if (gcov_var.file)
    {
      if (fclose (gcov_var.file))
	gcov_var.error = GCOV_FILE_WRITE_ERROR;

      gcov_var.file = 0;
    }
  gcov_var.mode = 0;
  return gcov_var.error;
}

#if !IN_LIBGCOV || defined (IN_GCOV_TOOL)
/* Check if MAGIC is EXPECTED. Use it to determine endianness of the
   file. Returns +1 for same endian, -1 for other endian and zero for
   not EXPECTED.  */

GCOV_LINKAGE int
gcov_magic (gcov_unsigned_t magic, gcov_unsigned_t expected)
{
  if (magic == expected)
    return 1;

  if (__builtin_bswap32 (magic) == expected)
    {
      gcov_var.endian = 1;
      return -1;
    }
  return 0;
}
#endif

#if !IN_GCOV
/* Write DATA of LENGTH characters to coverage file.  */

GCOV_LINKAGE void
gcov_write (const void *data, unsigned length)
{
  gcov_unsigned_t r = fwrite (data, length, 1, gcov_var.file);
  if (r != 1)
    gcov_var.error = GCOV_FILE_WRITE_ERROR;
}

/* Write unsigned VALUE to coverage file.  */

GCOV_LINKAGE void
gcov_write_unsigned (gcov_unsigned_t value)
{
  gcov_unsigned_t r = fwrite (&value, sizeof (value), 1, gcov_var.file);
  if (r != 1)
    gcov_var.error = GCOV_FILE_WRITE_ERROR;
}

#if !IN_LIBGCOV
/* Write STRING to coverage file.  Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE void
gcov_write_string (const char *string)
{
  unsigned length = 0;

  if (string)
    length = strlen (string) + 1;

  gcov_write_unsigned (length);
  if (length > 0)
    {
      gcov_unsigned_t r = fwrite (string, length, 1, gcov_var.file);
      if (r != 1)
	gcov_var.error = GCOV_FILE_WRITE_ERROR;
    }
}
#endif

#if !IN_LIBGCOV
/* Write FILENAME to coverage file.  Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE void
gcov_write_filename (const char *filename)
{
  if (profile_abs_path_flag && filename && filename[0]
      && !(IS_DIR_SEPARATOR (filename[0])
#if HAVE_DOS_BASED_FILE_SYSTEM
	   || filename[1] == ':'
#endif
	  ))
    {
      char *buf = getcwd (NULL, 0);
      if (buf != NULL && buf[0])
	{
	  size_t len = strlen (buf);
	  buf = (char*)xrealloc (buf, len + strlen (filename) + 2);
	  if (!IS_DIR_SEPARATOR (buf[len - 1]))
	    strcat (buf, "/");
	  strcat (buf, filename);
	  gcov_write_string (buf);
	  free (buf);
	  return;
	}
    }

  gcov_write_string (filename);
}

/* Move to a given position in a gcov file.  */

static void
gcov_seek (gcov_position_t base)
{
  fseek (gcov_var.file, base, SEEK_SET);
}

/* Write a tag TAG and reserve space for the record length. Return a
   value to be used for gcov_write_length.  */

GCOV_LINKAGE gcov_position_t
gcov_write_tag (gcov_unsigned_t tag)
{
  gcov_position_t result = gcov_position ();
  gcov_write_unsigned (tag);
  gcov_write_unsigned (0);

  return result;
}

/* Write a record length using POSITION, which was returned by
   gcov_write_tag.  The current file position is the end of the
   record, and is restored before returning.  Returns nonzero on
   overflow.  */

GCOV_LINKAGE void
gcov_write_length (gcov_position_t position)
{
  gcov_position_t current_position = gcov_position ();
  gcov_nonruntime_assert (gcov_var.mode < 0);
  gcov_nonruntime_assert (current_position >= position + 2 * GCOV_WORD_SIZE);

  gcov_seek (position + GCOV_WORD_SIZE);
  gcov_write_unsigned (current_position - position - 2 * GCOV_WORD_SIZE);
  gcov_seek (current_position);
}

#else /* IN_LIBGCOV */

/* Write an object summary structure to the gcov file.  */

GCOV_LINKAGE void
gcov_write_object_summary (const struct gcov_summary *summary)
{
  gcov_write_unsigned (GCOV_TAG_OBJECT_SUMMARY);
  gcov_write_unsigned (GCOV_TAG_OBJECT_SUMMARY_LENGTH);
  gcov_write_unsigned (summary->runs);
  gcov_write_unsigned (summary->sum_max);
}

#endif /* IN_LIBGCOV */

#endif /*!IN_GCOV */

/* Return a pointer to read COUNT bytes from the gcov file.  Returns
   NULL on failure (read past EOF).  */

static void *
gcov_read_bytes (void *buffer, unsigned count)
{
  if (gcov_var.mode <= 0)
    return NULL;

  unsigned read = fread (buffer, count, 1, gcov_var.file);
  if (read != 1)
    {
      if (feof (gcov_var.file))
	gcov_var.error = GCOV_FILE_EOF;
      return NULL;
    }

#ifdef IN_GCOV_TOOL
  gcov_var.pos += count;
#endif
  return buffer;
}

/* Read WORDS gcov_unsigned_t values from gcov file.  */

static gcov_unsigned_t *
gcov_read_words (void *buffer, unsigned words)
{
  return (gcov_unsigned_t *)gcov_read_bytes (buffer, GCOV_WORD_SIZE * words);
}

/* Read unsigned value from a coverage file. Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE gcov_unsigned_t
gcov_read_unsigned (void)
{
  gcov_unsigned_t value;
  gcov_unsigned_t allocated_buffer[1];
  gcov_unsigned_t *buffer = gcov_read_words (&allocated_buffer, 1);

  if (!buffer)
    return 0;

  value = from_file (buffer[0]);
  return value;
}

/* Read counter value from a coverage file. Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE gcov_type
gcov_read_counter (void)
{
  gcov_type value;
  gcov_unsigned_t allocated_buffer[2];
  gcov_unsigned_t *buffer = gcov_read_words (&allocated_buffer, 2);

  if (!buffer)
    return 0;
  value = from_file (buffer[0]);
  if (sizeof (value) > sizeof (gcov_unsigned_t))
    value |= ((gcov_type) from_file (buffer[1])) << 32;
  else if (buffer[1])
    gcov_var.error = GCOV_FILE_COUNTER_OVERFLOW;

  return value;
}

/* Mangle filename path of BASE and output new allocated pointer with
   mangled path.  */

char *
mangle_path (char const *base)
{
  /* Convert '/' to '#', convert '..' to '^',
     convert ':' to '~' on DOS based file system.  */
  const char *probe;
  char *buffer = (char *)xmalloc (strlen (base) + 1);
  char *ptr = buffer;

#if HAVE_DOS_BASED_FILE_SYSTEM
  if (base[0] && base[1] == ':')
    {
      ptr[0] = base[0];
      ptr[1] = '~';
      ptr += 2;
      base += 2;
    }
#endif
  for (; *base; base = probe)
    {
      size_t len;

      for (probe = base; *probe; probe++)
	if (*probe == '/')
	  break;
      len = probe - base;
      if (len == 2 && base[0] == '.' && base[1] == '.')
	*ptr++ = '^';
      else
	{
	  memcpy (ptr, base, len);
	  ptr += len;
	}
      if (*probe)
	{
	  *ptr++ = '#';
	  probe++;
	}
    }

  /* Terminate the string.  */
  *ptr = '\0';

  return buffer;
}

/* We need to expose the below function when compiling for gcov-tool.  */

#if !IN_LIBGCOV || defined (IN_GCOV_TOOL)
/* Read string from coverage file.  Allocate the buffer for the string
   from the heap or die.  Return a pointer to the string, or NULL on
   empty string.  */

GCOV_LINKAGE const char *
gcov_read_string (void)
{
  unsigned length = gcov_read_unsigned ();

  if (!length)
    return 0;

  void *buffer = XNEWVEC (char *, length);
  return (const char *) gcov_read_bytes (buffer, length);
}
#endif

GCOV_LINKAGE void
gcov_read_summary (struct gcov_summary *summary)
{
  summary->runs = gcov_read_unsigned ();
  summary->sum_max = gcov_read_unsigned ();
}

/* We need to expose the below function when compiling for gcov-tool.  */

#if !IN_LIBGCOV || defined (IN_GCOV_TOOL)
/* Reset to a known position.  BASE should have been obtained from
   gcov_position, LENGTH should be a record length.  */

GCOV_LINKAGE void
gcov_sync (gcov_position_t base, gcov_unsigned_t length)
{
  gcov_nonruntime_assert (gcov_var.mode > 0);
  base += length;
#ifdef IN_GCOV_TOOL
  if (gcov_var.mode == GCOV_MODE_STDIN)
    {
      while (gcov_var.pos < base)
	{
	  ++gcov_var.pos;
	  (void)fgetc (gcov_var.file);
	}
      return;
    }
#endif
  fseek (gcov_var.file, base, SEEK_SET);
}
#endif

#if IN_GCOV > 0
/* Return the modification time of the current gcov file.  */

GCOV_LINKAGE time_t
gcov_time (void)
{
  struct stat status;

  if (fstat (fileno (gcov_var.file), &status))
    return 0;
  else
    return status.st_mtime;
}
#endif /* IN_GCOV */
