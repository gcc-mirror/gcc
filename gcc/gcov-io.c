/* File format for coverage information
   Copyright (C) 1996, 1997, 1998, 2000, 2002,
   2003  Free Software Foundation, Inc.
   Contributed by Bob Manson <manson@cygnus.com>.
   Completely remangled by Nathan Sidwell <nathan@codesourcery.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Routines declared in gcov-io.h.  This file should be #included by
   another source file, after having #included gcov-io.h.  */

/* Open a gcov file. NAME is the name of the file to open and MODE
   indicates whether a new file should be created, or an existing file
   opened for modification. If MODE is >= 0 an existing file will be
   opened, if possible, and if MODE is <= 0, a new file will be
   created. Use MODE=0 to attempt to reopen an existing file and then
   fall back on creating a new one.  Return zero on failure, >0 on
   opening an existing file and <0 on creating a new one.  */

GCOV_LINKAGE int
gcov_open (const char *name, int mode)
{
  int result = 1;
  size_t alloc = 1024;
#if GCOV_LOCKED
  struct flock s_flock;

  s_flock.l_type = F_WRLCK;
  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0; /* Until EOF.  */
  s_flock.l_pid = getpid ();
#endif
  
  if (gcov_var.file)
    abort ();
  gcov_var.position = gcov_var.length = 0;
  gcov_var.error = gcov_var.modified = 0;
  if (mode >= 0)
    gcov_var.file = fopen (name, "r+b");
  if (!gcov_var.file && mode <= 0)
    {
      result = -1;
      gcov_var.file = fopen (name, "w+b");
    }
  if (!gcov_var.file)
    return 0;

#if GCOV_LOCKED
  while (fcntl (fileno (gcov_var.file), F_SETLKW, &s_flock)
	 && errno == EINTR)
    continue;
#endif

  if (result >= 0)
    {
      if (fseek (gcov_var.file, 0, SEEK_END))
	{
	  fclose (gcov_var.file);
	  gcov_var.file = 0;
	  return 0;
	}
      gcov_var.length = ftell (gcov_var.file);
      fseek (gcov_var.file, 0, SEEK_SET);
      alloc += gcov_var.length;
    }
  if (alloc > gcov_var.alloc)
    {
      if (gcov_var.buffer)
	free (gcov_var.buffer);
      gcov_var.alloc = alloc;
#if IN_LIBGCOV
      gcov_var.buffer = malloc (gcov_var.alloc);
      if (!gcov_var.buffer)
	{
	  fclose (gcov_var.file);
	  gcov_var.file = 0;
	  gcov_var.length = 0;
	  gcov_var.alloc = 0;
	  return 0;
	}
#else
      gcov_var.buffer = xmalloc (gcov_var.alloc);
#endif
    }
  if (result >= 0
      && fread (gcov_var.buffer, gcov_var.length, 1, gcov_var.file) != 1)
    {
      fclose (gcov_var.file);
      gcov_var.file = 0;
      gcov_var.length = 0;
      return 0;
    }
  return result;
}

/* Close the current gcov file. Flushes data to disk. Returns nonzero
   on failure or error flag set.  */

GCOV_LINKAGE int
gcov_close ()
{
  int result = 0;
  
  if (gcov_var.file)
    {
      if (gcov_var.modified
	  && (fseek (gcov_var.file, 0, SEEK_SET)
	      || fwrite (gcov_var.buffer, gcov_var.length,
			 1, gcov_var.file) != 1))
	result = 1;
      fclose (gcov_var.file);
      gcov_var.file = 0;
      gcov_var.length = 0;
    }
#if !IN_LIBGCOV
  free (gcov_var.buffer);
  gcov_var.alloc = 0;
  gcov_var.buffer = 0;
#endif
  return result ? 1 : gcov_var.error;
}

#if !IN_GCOV
/* Allocate space to write BYTES bytes to the gcov file. Return a
   pointer to those bytes, or NULL on failure.  */

GCOV_LINKAGE unsigned char *
gcov_write_bytes (unsigned bytes)
{
  char unsigned *result;

  if (gcov_var.position + bytes > gcov_var.alloc)
    {
      size_t new_size = (gcov_var.alloc + bytes) * 3 / 2;

      if (!gcov_var.buffer)
	return 0;
#if IN_LIBGCOV
      result = realloc (gcov_var.buffer, new_size);
      if (!result)
	{
	  free (gcov_var.buffer);
	  gcov_var.buffer = 0;
	  gcov_var.alloc = 0;
	  gcov_var.position = gcov_var.length = 0;
	  gcov_var.error = 1;
	  return 0;
	}
#else
      result = xrealloc (gcov_var.buffer, new_size);
#endif
      gcov_var.alloc = new_size;
      gcov_var.buffer = result;
    }
  
  result = &gcov_var.buffer[gcov_var.position];
  gcov_var.position += bytes;
  gcov_var.modified = 1;
  if (gcov_var.position > gcov_var.length)
    gcov_var.length = gcov_var.position;
  return result;
}

/* Write unsigned VALUE to coverage file.  Sets error flag
   appropriately.  */

GCOV_LINKAGE void
gcov_write_unsigned (unsigned value)
{
  unsigned char *buffer = gcov_write_bytes (4);
  unsigned ix;

  if (!buffer)
    return;
  for (ix = 4; ix--; )
    {
      buffer[ix] = value;
      value >>= 8;
    }
  if (sizeof (value) > 4 && value)
    gcov_var.error = -1;

  return;
}

/* Write counter VALUE to coverage file.  Sets error flag
   appropriately.  */

#if IN_LIBGCOV
GCOV_LINKAGE void
gcov_write_counter (gcov_type value)
{
  unsigned char *buffer = gcov_write_bytes (8);
  unsigned ix;

  if (!buffer)
    return;
  for (ix = 8; ix--; )
    {
      buffer[ix] = value;
      value >>= 8;
    }
  if ((sizeof (value) > 8 && value) || value < 0)
    gcov_var.error = -1;
  return;
}
#endif /* IN_LIBGCOV */

#if !IN_LIBGCOV
/* Write STRING to coverage file.  Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE void
gcov_write_string (const char *string)
{
  unsigned length = 0;
  unsigned pad = 0;
  unsigned rem = 0;
  unsigned char *buffer;

  if (string)
    {
      length = strlen (string);
      rem = 4 - (length & 3);
    }
  
  buffer = gcov_write_bytes (4 + length + rem);
  if (buffer)
    {
      unsigned ix;
      unsigned value = length;
      
      for (ix = 4; ix--; )
	{
	  buffer[ix] = value;
	  value >>= 8;
	}
      memcpy (buffer + 4, string, length);
      memcpy (buffer + 4 + length, &pad, rem);
    }
}
#endif

#if !IN_LIBGCOV
/* Write a tag TAG and reserve space for the record length. Return a
   value to be used for gcov_write_length.  */

GCOV_LINKAGE unsigned long
gcov_write_tag (unsigned tag)
{
  unsigned long result = gcov_var.position;
  unsigned char *buffer = gcov_write_bytes (8);
  unsigned ix;

  if (!buffer)
    return 0;
  for (ix = 4; ix--; )
    {
      buffer[ix] = tag;
      tag >>= 8;
    }
  memset (buffer + 4, 0, 4);
  return result;
}

/* Write a record length using POSITION, which was returned by
   gcov_write_tag.  The current file position is the end of the
   record, and is restored before returning.  Returns nonzero on
   overflow.  */

GCOV_LINKAGE void
gcov_write_length (unsigned long position)
{
  if (position)
    {
      unsigned length = gcov_var.position - position - 8;
      unsigned char *buffer = &gcov_var.buffer[position + 4];
      unsigned ix;
      
      for (ix = 4; ix--; )
	{
	  buffer[ix] = length;
	  length >>= 8;
	}
    }
}
#endif

/* Write a tag TAG and length LENGTH.  */

GCOV_LINKAGE void
gcov_write_tag_length (unsigned tag, unsigned length)
{
  unsigned char *buffer = gcov_write_bytes (8);
  unsigned ix;

  if (!buffer)
    return;
  for (ix = 4; ix--; )
    {
      buffer[ix] = tag;
      tag >>= 8;
    }
  for (ix = 4; ix--; )
    {
      buffer[ix + 4] = length;
      length >>= 8;
    }
  return;
}

#if IN_LIBGCOV
/* Write a summary structure to the gcov file.  Return non-zero on
   overflow.  */

GCOV_LINKAGE void
gcov_write_summary (unsigned tag, const struct gcov_summary *summary)
{
  unsigned ix;
  const struct gcov_ctr_summary *csum;

  gcov_write_tag_length (tag, GCOV_TAG_SUMMARY_LENGTH);
  gcov_write_unsigned (summary->checksum);
  for (csum = summary->ctrs, ix = GCOV_COUNTERS; ix--; csum++)
    {
      gcov_write_unsigned (csum->num);
      gcov_write_unsigned (csum->runs);
      gcov_write_counter (csum->sum_all);
      gcov_write_counter (csum->run_max);
      gcov_write_counter (csum->sum_max);
    }
}
#endif /* IN_LIBGCOV */

#endif /*!IN_GCOV */

/* Return a pointer to read BYTES bytes from the gcov file. Returns
   NULL on failure (read past EOF). */

GCOV_LINKAGE const unsigned char *
gcov_read_bytes (unsigned bytes)
{
  const unsigned char *result;
  
  if (gcov_var.position + bytes > gcov_var.length)
    {
      gcov_var.error = 1;
      return 0;
    }
  
  result = &gcov_var.buffer[gcov_var.position];
  gcov_var.position += bytes;
  return result;
}

/* Read unsigned value from a coverage file. Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE unsigned
gcov_read_unsigned ()
{
  unsigned value = 0;
  unsigned ix;
  const unsigned char *buffer = gcov_read_bytes (4);

  if (!buffer)
    return 0;
  for (ix = sizeof (value); ix < 4; ix++)
    if (buffer[ix])
      gcov_var.error = -1;
  for (ix = 0; ix != 4; ix++)
    {
      value <<= 8;
      value |= buffer[ix];
    }
  return value;
}

/* Read counter value from a coverage file. Sets error flag on file
   error, overflow flag on overflow */

GCOV_LINKAGE gcov_type
gcov_read_counter ()
{
  gcov_type value = 0;
  unsigned ix;
  const unsigned char *buffer = gcov_read_bytes (8);

  if (!buffer)
    return 0;
  for (ix = sizeof (value); ix < 8; ix++)
    if (buffer[ix])
      gcov_var.error = -1;
  for (ix = 0; ix != 8; ix++)
    {
      value <<= 8;
      value |= buffer[ix];
    }
  if (value < 0)
    gcov_var.error = -1;
  return value;
}

/* Read string from coverage file. Returns a pointer to a static
   buffer, or NULL on empty string. You must copy the string before
   calling another gcov function.  */

#if !IN_LIBGCOV
GCOV_LINKAGE const char *
gcov_read_string ()
{
  unsigned length = gcov_read_unsigned ();
  
  if (!length)
    return 0;

  length += 4 - (length & 3);
  return (const char *) gcov_read_bytes (length);
}
#endif

GCOV_LINKAGE void
gcov_read_summary (struct gcov_summary *summary)
{
  unsigned ix;
  struct gcov_ctr_summary *csum;
  
  summary->checksum = gcov_read_unsigned ();
  for (csum = summary->ctrs, ix = GCOV_COUNTERS; ix--; csum++)
    {
      csum->num = gcov_read_unsigned ();
      csum->runs = gcov_read_unsigned ();
      csum->sum_all = gcov_read_counter ();
      csum->run_max = gcov_read_counter ();
      csum->sum_max = gcov_read_counter ();
    }
}

#if IN_GCOV > 0
/* Return the modification time of the current gcov file.  */

GCOV_LINKAGE time_t
gcov_time ()
{
  struct stat status;
  
  if (fstat (fileno (gcov_var.file), &status))
    return 0;
  else
    return status.st_mtime;
}
#endif /* IN_GCOV */
