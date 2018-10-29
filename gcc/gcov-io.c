/* File format for coverage information
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
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

#if !IN_GCOV
static void gcov_write_block (unsigned);
static gcov_unsigned_t *gcov_write_words (unsigned);
#endif
static const gcov_unsigned_t *gcov_read_words (unsigned);
#if !IN_LIBGCOV
static void gcov_allocate (unsigned);
#endif

/* Optimum number of gcov_unsigned_t's read from or written to disk.  */
#define GCOV_BLOCK_SIZE (1 << 10)

struct gcov_var
{
  FILE *file;
  gcov_position_t start;	/* Position of first byte of block */
  unsigned offset;		/* Read/write position within the block.  */
  unsigned length;		/* Read limit in the block.  */
  unsigned overread;		/* Number of words overread.  */
  int error;			/* < 0 overflow, > 0 disk error.  */
  int mode;	                /* < 0 writing, > 0 reading */
#if IN_LIBGCOV
  /* Holds one block plus 4 bytes, thus all coverage reads & writes
     fit within this buffer and we always can transfer GCOV_BLOCK_SIZE
     to and from the disk. libgcov never backtracks and only writes 4
     or 8 byte objects.  */
  gcov_unsigned_t buffer[GCOV_BLOCK_SIZE + 1];
#else
  int endian;			/* Swap endianness.  */
  /* Holds a variable length block, as the compiler can write
     strings and needs to backtrack.  */
  size_t alloc;
  gcov_unsigned_t *buffer;
#endif
} gcov_var;

/* Save the current position in the gcov file.  */
/* We need to expose this function when compiling for gcov-tool.  */
#ifndef IN_GCOV_TOOL
static inline
#endif
gcov_position_t
gcov_position (void)
{
  gcov_nonruntime_assert (gcov_var.mode > 0); 
  return gcov_var.start + gcov_var.offset;
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
/* Move to beginning of file and initialize for writing.  */
GCOV_LINKAGE inline void
gcov_rewrite (void)
{
  gcov_var.mode = -1; 
  gcov_var.start = 0;
  gcov_var.offset = 0;
  fseek (gcov_var.file, 0L, SEEK_SET);
}
#endif

static inline gcov_unsigned_t from_file (gcov_unsigned_t value)
{
#if !IN_LIBGCOV
  if (gcov_var.endian)
    {
      value = (value >> 16) | (value << 16);
      value = ((value & 0xff00ff) << 8) | ((value >> 8) & 0xff00ff);
    }
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
#if IN_LIBGCOV
gcov_open (const char *name)
#else
gcov_open (const char *name, int mode)
#endif
{
#if IN_LIBGCOV
  int mode = 0;
#endif
#if GCOV_LOCKED
  struct flock s_flock;
  int fd;

  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0; /* Until EOF.  */
  s_flock.l_pid = getpid ();
#endif

  gcov_nonruntime_assert (!gcov_var.file);
  gcov_var.start = 0;
  gcov_var.offset = gcov_var.length = 0;
  gcov_var.overread = -1u;
  gcov_var.error = 0;
#if !IN_LIBGCOV
  gcov_var.endian = 0;
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

  setbuf (gcov_var.file, (char *)0);

  return 1;
}

/* Close the current gcov file. Flushes data to disk. Returns nonzero
   on failure or error flag set.  */

GCOV_LINKAGE int
gcov_close (void)
{
  if (gcov_var.file)
    {
#if !IN_GCOV
      if (gcov_var.offset && gcov_var.mode < 0)
	gcov_write_block (gcov_var.offset);
#endif
      fclose (gcov_var.file);
      gcov_var.file = 0;
      gcov_var.length = 0;
    }
#if !IN_LIBGCOV
  free (gcov_var.buffer);
  gcov_var.alloc = 0;
  gcov_var.buffer = 0;
#endif
  gcov_var.mode = 0;
  return gcov_var.error;
}

#if !IN_LIBGCOV
/* Check if MAGIC is EXPECTED. Use it to determine endianness of the
   file. Returns +1 for same endian, -1 for other endian and zero for
   not EXPECTED.  */

GCOV_LINKAGE int
gcov_magic (gcov_unsigned_t magic, gcov_unsigned_t expected)
{
  if (magic == expected)
    return 1;
  magic = (magic >> 16) | (magic << 16);
  magic = ((magic & 0xff00ff) << 8) | ((magic >> 8) & 0xff00ff);
  if (magic == expected)
    {
      gcov_var.endian = 1;
      return -1;
    }
  return 0;
}
#endif

#if !IN_LIBGCOV
static void
gcov_allocate (unsigned length)
{
  size_t new_size = gcov_var.alloc;

  if (!new_size)
    new_size = GCOV_BLOCK_SIZE;
  new_size += length;
  new_size *= 2;

  gcov_var.alloc = new_size;
  gcov_var.buffer = XRESIZEVAR (gcov_unsigned_t, gcov_var.buffer, new_size << 2);
}
#endif

#if !IN_GCOV
/* Write out the current block, if needs be.  */

static void
gcov_write_block (unsigned size)
{
  if (fwrite (gcov_var.buffer, size << 2, 1, gcov_var.file) != 1)
    gcov_var.error = 1;
  gcov_var.start += size;
  gcov_var.offset -= size;
}

/* Allocate space to write BYTES bytes to the gcov file. Return a
   pointer to those bytes, or NULL on failure.  */

static gcov_unsigned_t *
gcov_write_words (unsigned words)
{
  gcov_unsigned_t *result;

  gcov_nonruntime_assert (gcov_var.mode < 0);
#if IN_LIBGCOV
  if (gcov_var.offset >= GCOV_BLOCK_SIZE)
    {
      gcov_write_block (GCOV_BLOCK_SIZE);
      if (gcov_var.offset)
	{
	  memcpy (gcov_var.buffer, gcov_var.buffer + GCOV_BLOCK_SIZE, 4);
	}
    }
#else
  if (gcov_var.offset + words > gcov_var.alloc)
    gcov_allocate (gcov_var.offset + words);
#endif
  result = &gcov_var.buffer[gcov_var.offset];
  gcov_var.offset += words;

  return result;
}

/* Write unsigned VALUE to coverage file.  Sets error flag
   appropriately.  */

GCOV_LINKAGE void
gcov_write_unsigned (gcov_unsigned_t value)
{
  gcov_unsigned_t *buffer = gcov_write_words (1);

  buffer[0] = value;
}

/* Write counter VALUE to coverage file.  Sets error flag
   appropriately.  */

#if IN_LIBGCOV
GCOV_LINKAGE void
gcov_write_counter (gcov_type value)
{
  gcov_unsigned_t *buffer = gcov_write_words (2);

  buffer[0] = (gcov_unsigned_t) value;
  if (sizeof (value) > sizeof (gcov_unsigned_t))
    buffer[1] = (gcov_unsigned_t) (value >> 32);
  else
    buffer[1] = 0;
}
#endif /* IN_LIBGCOV */

#if !IN_LIBGCOV
/* Write STRING to coverage file.  Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE void
gcov_write_string (const char *string)
{
  unsigned length = 0;
  unsigned alloc = 0;
  gcov_unsigned_t *buffer;

  if (string)
    {
      length = strlen (string);
      alloc = (length + 4) >> 2;
    }

  buffer = gcov_write_words (1 + alloc);

  buffer[0] = alloc;

  if (alloc > 0)
    {
      buffer[alloc] = 0; /* place nul terminators.  */
      memcpy (&buffer[1], string, length);
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
#endif

#if !IN_LIBGCOV
/* Write a tag TAG and reserve space for the record length. Return a
   value to be used for gcov_write_length.  */

GCOV_LINKAGE gcov_position_t
gcov_write_tag (gcov_unsigned_t tag)
{
  gcov_position_t result = gcov_var.start + gcov_var.offset;
  gcov_unsigned_t *buffer = gcov_write_words (2);

  buffer[0] = tag;
  buffer[1] = 0;

  return result;
}

/* Write a record length using POSITION, which was returned by
   gcov_write_tag.  The current file position is the end of the
   record, and is restored before returning.  Returns nonzero on
   overflow.  */

GCOV_LINKAGE void
gcov_write_length (gcov_position_t position)
{
  unsigned offset;
  gcov_unsigned_t length;
  gcov_unsigned_t *buffer;

  gcov_nonruntime_assert (gcov_var.mode < 0);
  gcov_nonruntime_assert (position + 2 <= gcov_var.start + gcov_var.offset);
  gcov_nonruntime_assert (position >= gcov_var.start);
  offset = position - gcov_var.start;
  length = gcov_var.offset - offset - 2;
  buffer = (gcov_unsigned_t *) &gcov_var.buffer[offset];
  buffer[1] = length;
  if (gcov_var.offset >= GCOV_BLOCK_SIZE)
    gcov_write_block (gcov_var.offset);
}

#else /* IN_LIBGCOV */

/* Write a tag TAG and length LENGTH.  */

GCOV_LINKAGE void
gcov_write_tag_length (gcov_unsigned_t tag, gcov_unsigned_t length)
{
  gcov_unsigned_t *buffer = gcov_write_words (2);

  buffer[0] = tag;
  buffer[1] = length;
}

/* Write a summary structure to the gcov file.  Return nonzero on
   overflow.  */

GCOV_LINKAGE void
gcov_write_summary (gcov_unsigned_t tag, const struct gcov_summary *summary)
{
  gcov_write_tag_length (tag, GCOV_TAG_SUMMARY_LENGTH);
  gcov_write_unsigned (summary->runs);
  gcov_write_unsigned (summary->sum_max);
}

#endif /* IN_LIBGCOV */

#endif /*!IN_GCOV */

/* Return a pointer to read BYTES bytes from the gcov file. Returns
   NULL on failure (read past EOF).  */

static const gcov_unsigned_t *
gcov_read_words (unsigned words)
{
  const gcov_unsigned_t *result;
  unsigned excess = gcov_var.length - gcov_var.offset;

  if (gcov_var.mode <= 0)
    return NULL;

  if (excess < words)
    {
      gcov_var.start += gcov_var.offset;
      if (excess)
	{
#if IN_LIBGCOV
	  memcpy (gcov_var.buffer, gcov_var.buffer + gcov_var.offset, 4);
#else
	  memmove (gcov_var.buffer, gcov_var.buffer + gcov_var.offset,
		   excess * 4);
#endif
	}
      gcov_var.offset = 0;
      gcov_var.length = excess;
#if IN_LIBGCOV
      excess = GCOV_BLOCK_SIZE;
#else
      if (gcov_var.length + words > gcov_var.alloc)
	gcov_allocate (gcov_var.length + words);
      excess = gcov_var.alloc - gcov_var.length;
#endif
      excess = fread (gcov_var.buffer + gcov_var.length,
		      1, excess << 2, gcov_var.file) >> 2;
      gcov_var.length += excess;
      if (gcov_var.length < words)
	{
	  gcov_var.overread += words - gcov_var.length;
	  gcov_var.length = 0;
	  return 0;
	}
    }
  result = &gcov_var.buffer[gcov_var.offset];
  gcov_var.offset += words;
  return result;
}

/* Read unsigned value from a coverage file. Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE gcov_unsigned_t
gcov_read_unsigned (void)
{
  gcov_unsigned_t value;
  const gcov_unsigned_t *buffer = gcov_read_words (1);

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
  const gcov_unsigned_t *buffer = gcov_read_words (2);

  if (!buffer)
    return 0;
  value = from_file (buffer[0]);
  if (sizeof (value) > sizeof (gcov_unsigned_t))
    value |= ((gcov_type) from_file (buffer[1])) << 32;
  else if (buffer[1])
    gcov_var.error = -1;

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
  char *buffer = (char *)xmalloc (strlen (base) + 10);
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
/* Read string from coverage file. Returns a pointer to a static
   buffer, or NULL on empty string. You must copy the string before
   calling another gcov function.  */

GCOV_LINKAGE const char *
gcov_read_string (void)
{
  unsigned length = gcov_read_unsigned ();

  if (!length)
    return 0;

  return (const char *) gcov_read_words (length);
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
  if (base - gcov_var.start <= gcov_var.length)
    gcov_var.offset = base - gcov_var.start;
  else
    {
      gcov_var.offset = gcov_var.length = 0;
      fseek (gcov_var.file, base << 2, SEEK_SET);
      gcov_var.start = ftell (gcov_var.file) >> 2;
    }
}
#endif

#if IN_LIBGCOV
/* Move to a given position in a gcov file.  */

GCOV_LINKAGE void
gcov_seek (gcov_position_t base)
{
  if (gcov_var.offset)
    gcov_write_block (gcov_var.offset);
  fseek (gcov_var.file, base << 2, SEEK_SET);
  gcov_var.start = ftell (gcov_var.file) >> 2;
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
