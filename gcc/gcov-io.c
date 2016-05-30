/* File format for coverage information
   Copyright (C) 1996-2016 Free Software Foundation, Inc.
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
   creating a new one.  If MODE < 0, the file will be opened in
   read-only mode.  Otherwise it will be opened for modification.
   Return zero on failure, >0 on opening an existing file and <0 on
   creating a new one.  */

GCOV_LINKAGE int
#if IN_LIBGCOV
gcov_open (const char *name)
#else
gcov_open (const char *name, int mode)
#endif
{
#if IN_LIBGCOV
  const int mode = 0;
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
  else if (mode < 0)
     {
       /* Write mode - acquire a write-lock.  */
       s_flock.l_type = F_WRLCK;
      fd = open (name, O_RDWR | O_CREAT | O_TRUNC, 0666);
    }
  else /* mode == 0 */
    {
      /* Read-Write mode - acquire a write-lock.  */
      s_flock.l_type = F_WRLCK;
      fd = open (name, O_RDWR | O_CREAT, 0666);
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

  if (mode > 0)
    gcov_var.mode = 1;
  else if (mode == 0)
    {
      struct stat st;

      if (fstat (fd, &st) < 0)
	{
	  fclose (gcov_var.file);
	  gcov_var.file = 0;
	  return 0;
	}
      if (st.st_size != 0)
	gcov_var.mode = 1;
      else
	gcov_var.mode = mode * 2 + 1;
    }
  else
    gcov_var.mode = mode * 2 + 1;
#else
  if (mode >= 0)
    gcov_var.file = fopen (name, (mode > 0) ? "rb" : "r+b");

  if (gcov_var.file)
    gcov_var.mode = 1;
  else if (mode <= 0)
    {
      gcov_var.file = fopen (name, "w+b");
      if (gcov_var.file)
	gcov_var.mode = mode * 2 + 1;
    }
  if (!gcov_var.file)
    return 0;
#endif

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
  buffer[alloc] = 0;
  memcpy (&buffer[1], string, length);
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
  unsigned ix, h_ix, bv_ix, h_cnt = 0;
  const struct gcov_ctr_summary *csum;
  unsigned histo_bitvector[GCOV_HISTOGRAM_BITVECTOR_SIZE];

  /* Count number of non-zero histogram entries, and fill in a bit vector
     of non-zero indices. The histogram is only currently computed for arc
     counters.  */
  for (bv_ix = 0; bv_ix < GCOV_HISTOGRAM_BITVECTOR_SIZE; bv_ix++)
    histo_bitvector[bv_ix] = 0;
  csum = &summary->ctrs[GCOV_COUNTER_ARCS];
  for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
    {
      if (csum->histogram[h_ix].num_counters > 0)
        {
          histo_bitvector[h_ix / 32] |= 1 << (h_ix % 32);
          h_cnt++;
        }
    }
  gcov_write_tag_length (tag, GCOV_TAG_SUMMARY_LENGTH (h_cnt));
  gcov_write_unsigned (summary->checksum);
  for (csum = summary->ctrs, ix = GCOV_COUNTERS_SUMMABLE; ix--; csum++)
    {
      gcov_write_unsigned (csum->num);
      gcov_write_unsigned (csum->runs);
      gcov_write_counter (csum->sum_all);
      gcov_write_counter (csum->run_max);
      gcov_write_counter (csum->sum_max);
      if (ix != GCOV_COUNTER_ARCS)
        {
          for (bv_ix = 0; bv_ix < GCOV_HISTOGRAM_BITVECTOR_SIZE; bv_ix++)
            gcov_write_unsigned (0);
          continue;
        }
      for (bv_ix = 0; bv_ix < GCOV_HISTOGRAM_BITVECTOR_SIZE; bv_ix++)
        gcov_write_unsigned (histo_bitvector[bv_ix]);
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        {
          if (!csum->histogram[h_ix].num_counters)
            continue;
          gcov_write_unsigned (csum->histogram[h_ix].num_counters);
          gcov_write_counter (csum->histogram[h_ix].min_value);
          gcov_write_counter (csum->histogram[h_ix].cum_value);
        }
    }
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
  unsigned ix, h_ix, bv_ix, h_cnt = 0;
  struct gcov_ctr_summary *csum;
  unsigned histo_bitvector[GCOV_HISTOGRAM_BITVECTOR_SIZE];
  unsigned cur_bitvector;

  summary->checksum = gcov_read_unsigned ();
  for (csum = summary->ctrs, ix = GCOV_COUNTERS_SUMMABLE; ix--; csum++)
    {
      csum->num = gcov_read_unsigned ();
      csum->runs = gcov_read_unsigned ();
      csum->sum_all = gcov_read_counter ();
      csum->run_max = gcov_read_counter ();
      csum->sum_max = gcov_read_counter ();
      memset (csum->histogram, 0,
              sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);
      for (bv_ix = 0; bv_ix < GCOV_HISTOGRAM_BITVECTOR_SIZE; bv_ix++)
        {
          histo_bitvector[bv_ix] = gcov_read_unsigned ();
#if IN_LIBGCOV
          /* When building libgcov we don't include system.h, which includes
             hwint.h (where popcount_hwi is declared). However, libgcov.a
             is built by the bootstrapped compiler and therefore the builtins
             are always available.  */
          h_cnt += __builtin_popcount (histo_bitvector[bv_ix]);
#else
          h_cnt += popcount_hwi (histo_bitvector[bv_ix]);
#endif
        }
      bv_ix = 0;
      h_ix = 0;
      cur_bitvector = 0;
      while (h_cnt--)
        {
          /* Find the index corresponding to the next entry we will read in.
             First find the next non-zero bitvector and re-initialize
             the histogram index accordingly, then right shift and increment
             the index until we find a set bit.  */
          while (!cur_bitvector)
            {
              h_ix = bv_ix * 32;
              if (bv_ix >= GCOV_HISTOGRAM_BITVECTOR_SIZE)
                gcov_error ("corrupted profile info: summary histogram "
                            "bitvector is corrupt");
              cur_bitvector = histo_bitvector[bv_ix++];
            }
          while (!(cur_bitvector & 0x1))
            {
              h_ix++;
              cur_bitvector >>= 1;
            }
          if (h_ix >= GCOV_HISTOGRAM_SIZE)
            gcov_error ("corrupted profile info: summary histogram "
                        "index is corrupt");

          csum->histogram[h_ix].num_counters = gcov_read_unsigned ();
          csum->histogram[h_ix].min_value = gcov_read_counter ();
          csum->histogram[h_ix].cum_value = gcov_read_counter ();
          /* Shift off the index we are done with and increment to the
             corresponding next histogram entry.  */
          cur_bitvector >>= 1;
          h_ix++;
        }
    }
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

#if !IN_GCOV
/* Determine the index into histogram for VALUE. */

#if IN_LIBGCOV
static unsigned
#else
GCOV_LINKAGE unsigned
#endif
gcov_histo_index (gcov_type value)
{
  gcov_type_unsigned v = (gcov_type_unsigned)value;
  unsigned r = 0;
  unsigned prev2bits = 0;

  /* Find index into log2 scale histogram, where each of the log2
     sized buckets is divided into 4 linear sub-buckets for better
     focus in the higher buckets.  */

  /* Find the place of the most-significant bit set.  */
  if (v > 0)
    {
#if IN_LIBGCOV
      /* When building libgcov we don't include system.h, which includes
         hwint.h (where floor_log2 is declared). However, libgcov.a
         is built by the bootstrapped compiler and therefore the builtins
         are always available.  */
      r = sizeof (long long) * __CHAR_BIT__ - 1 - __builtin_clzll (v);
#else
      /* We use floor_log2 from hwint.c, which takes a HOST_WIDE_INT
         that is 64 bits and gcov_type_unsigned is 64 bits.  */
      r = floor_log2 (v);
#endif
    }

  /* If at most the 2 least significant bits are set (value is
     0 - 3) then that value is our index into the lowest set of
     four buckets.  */
  if (r < 2)
    return (unsigned)value;

  gcov_nonruntime_assert (r < 64);

  /* Find the two next most significant bits to determine which
     of the four linear sub-buckets to select.  */
  prev2bits = (v >> (r - 2)) & 0x3;
  /* Finally, compose the final bucket index from the log2 index and
     the next 2 bits. The minimum r value at this point is 2 since we
     returned above if r was 2 or more, so the minimum bucket at this
     point is 4.  */
  return (r - 1) * 4 + prev2bits;
}

/* Merge SRC_HISTO into TGT_HISTO. The counters are assumed to be in
   the same relative order in both histograms, and are matched up
   and merged in reverse order. Each counter is assigned an equal portion of
   its entry's original cumulative counter value when computing the
   new merged cum_value.  */

static void gcov_histogram_merge (gcov_bucket_type *tgt_histo,
                                  gcov_bucket_type *src_histo)
{
  int src_i, tgt_i, tmp_i = 0;
  unsigned src_num, tgt_num, merge_num;
  gcov_type src_cum, tgt_cum, merge_src_cum, merge_tgt_cum, merge_cum;
  gcov_type merge_min;
  gcov_bucket_type tmp_histo[GCOV_HISTOGRAM_SIZE];
  int src_done = 0;

  memset (tmp_histo, 0, sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);

  /* Assume that the counters are in the same relative order in both
     histograms. Walk the histograms from largest to smallest entry,
     matching up and combining counters in order.  */
  src_num = 0;
  src_cum = 0;
  src_i = GCOV_HISTOGRAM_SIZE - 1;
  for (tgt_i = GCOV_HISTOGRAM_SIZE - 1; tgt_i >= 0 && !src_done; tgt_i--)
    {
      tgt_num = tgt_histo[tgt_i].num_counters;
      tgt_cum = tgt_histo[tgt_i].cum_value;
      /* Keep going until all of the target histogram's counters at this
         position have been matched and merged with counters from the
         source histogram.  */
      while (tgt_num > 0 && !src_done)
        {
          /* If this is either the first time through this loop or we just
             exhausted the previous non-zero source histogram entry, look
             for the next non-zero source histogram entry.  */
          if (!src_num)
            {
              /* Locate the next non-zero entry.  */
              while (src_i >= 0 && !src_histo[src_i].num_counters)
                src_i--;
              /* If source histogram has fewer counters, then just copy over the
                 remaining target counters and quit.  */
              if (src_i < 0)
                {
                  tmp_histo[tgt_i].num_counters += tgt_num;
                  tmp_histo[tgt_i].cum_value += tgt_cum;
                  if (!tmp_histo[tgt_i].min_value ||
                      tgt_histo[tgt_i].min_value < tmp_histo[tgt_i].min_value)
                    tmp_histo[tgt_i].min_value = tgt_histo[tgt_i].min_value;
                  while (--tgt_i >= 0)
                    {
                      tmp_histo[tgt_i].num_counters
                          += tgt_histo[tgt_i].num_counters;
                      tmp_histo[tgt_i].cum_value += tgt_histo[tgt_i].cum_value;
                      if (!tmp_histo[tgt_i].min_value ||
                          tgt_histo[tgt_i].min_value
                          < tmp_histo[tgt_i].min_value)
                        tmp_histo[tgt_i].min_value = tgt_histo[tgt_i].min_value;
                    }

                  src_done = 1;
                  break;
                }

              src_num = src_histo[src_i].num_counters;
              src_cum = src_histo[src_i].cum_value;
            }

          /* The number of counters to merge on this pass is the minimum
             of the remaining counters from the current target and source
             histogram entries.  */
          merge_num = tgt_num;
          if (src_num < merge_num)
            merge_num = src_num;

          /* The merged min_value is the sum of the min_values from target
             and source.  */
          merge_min = tgt_histo[tgt_i].min_value + src_histo[src_i].min_value;

          /* Compute the portion of source and target entries' cum_value
             that will be apportioned to the counters being merged.
             The total remaining cum_value from each entry is divided
             equally among the counters from that histogram entry if we
             are not merging all of them.  */
          merge_src_cum = src_cum;
          if (merge_num < src_num)
            merge_src_cum = merge_num * src_cum / src_num;
          merge_tgt_cum = tgt_cum;
          if (merge_num < tgt_num)
            merge_tgt_cum = merge_num * tgt_cum / tgt_num;
          /* The merged cum_value is the sum of the source and target
             components.  */
          merge_cum = merge_src_cum + merge_tgt_cum;

          /* Update the remaining number of counters and cum_value left
             to be merged from this source and target entry.  */
          src_cum -= merge_src_cum;
          tgt_cum -= merge_tgt_cum;
          src_num -= merge_num;
          tgt_num -= merge_num;

          /* The merged counters get placed in the new merged histogram
             at the entry for the merged min_value.  */
          tmp_i = gcov_histo_index (merge_min);
          gcov_nonruntime_assert (tmp_i < GCOV_HISTOGRAM_SIZE);
          tmp_histo[tmp_i].num_counters += merge_num;
          tmp_histo[tmp_i].cum_value += merge_cum;
          if (!tmp_histo[tmp_i].min_value ||
              merge_min < tmp_histo[tmp_i].min_value)
            tmp_histo[tmp_i].min_value = merge_min;

          /* Ensure the search for the next non-zero src_histo entry starts
             at the next smallest histogram bucket.  */
          if (!src_num)
            src_i--;
        }
    }

  gcov_nonruntime_assert (tgt_i < 0);

  /* In the case where there were more counters in the source histogram,
     accumulate the remaining unmerged cumulative counter values. Add
     those to the smallest non-zero target histogram entry. Otherwise,
     the total cumulative counter values in the histogram will be smaller
     than the sum_all stored in the summary, which will complicate
     computing the working set information from the histogram later on.  */
  if (src_num)
    src_i--;
  while (src_i >= 0)
    {
      src_cum += src_histo[src_i].cum_value;
      src_i--;
    }
  /* At this point, tmp_i should be the smallest non-zero entry in the
     tmp_histo.  */
  gcov_nonruntime_assert (tmp_i >= 0 && tmp_i < GCOV_HISTOGRAM_SIZE
                          && tmp_histo[tmp_i].num_counters > 0);
  tmp_histo[tmp_i].cum_value += src_cum;

  /* Finally, copy the merged histogram into tgt_histo.  */
  memcpy (tgt_histo, tmp_histo,
	  sizeof (gcov_bucket_type) * GCOV_HISTOGRAM_SIZE);
}
#endif /* !IN_GCOV */

/* This is used by gcov-dump (IN_GCOV == -1) and in the compiler
   (!IN_GCOV && !IN_LIBGCOV).  */
#if IN_GCOV <= 0 && !IN_LIBGCOV
/* Compute the working set information from the counter histogram in
   the profile summary. This is an array of information corresponding to a
   range of percentages of the total execution count (sum_all), and includes
   the number of counters required to cover that working set percentage and
   the minimum counter value in that working set.  */

GCOV_LINKAGE void
compute_working_sets (const struct gcov_ctr_summary *summary,
                      gcov_working_set_t *gcov_working_sets)
{
  gcov_type working_set_cum_values[NUM_GCOV_WORKING_SETS];
  gcov_type ws_cum_hotness_incr;
  gcov_type cum, tmp_cum;
  const gcov_bucket_type *histo_bucket;
  unsigned ws_ix, c_num, count;
  int h_ix;

  /* Compute the amount of sum_all that the cumulative hotness grows
     by in each successive working set entry, which depends on the
     number of working set entries.  */
  ws_cum_hotness_incr = summary->sum_all / NUM_GCOV_WORKING_SETS;

  /* Next fill in an array of the cumulative hotness values corresponding
     to each working set summary entry we are going to compute below.
     Skip 0% statistics, which can be extrapolated from the
     rest of the summary data.  */
  cum = ws_cum_hotness_incr;
  for (ws_ix = 0; ws_ix < NUM_GCOV_WORKING_SETS;
       ws_ix++, cum += ws_cum_hotness_incr)
    working_set_cum_values[ws_ix] = cum;
  /* The last summary entry is reserved for (roughly) 99.9% of the
     working set. Divide by 1024 so it becomes a shift, which gives
     almost exactly 99.9%.  */
  working_set_cum_values[NUM_GCOV_WORKING_SETS-1]
      = summary->sum_all - summary->sum_all/1024;

  /* Next, walk through the histogram in decending order of hotness
     and compute the statistics for the working set summary array.
     As histogram entries are accumulated, we check to see which
     working set entries have had their expected cum_value reached
     and fill them in, walking the working set entries in increasing
     size of cum_value.  */
  ws_ix = 0; /* The current entry into the working set array.  */
  cum = 0; /* The current accumulated counter sum.  */
  count = 0; /* The current accumulated count of block counters.  */
  for (h_ix = GCOV_HISTOGRAM_SIZE - 1;
       h_ix >= 0 && ws_ix < NUM_GCOV_WORKING_SETS; h_ix--)
    {
      histo_bucket = &summary->histogram[h_ix];

      /* If we haven't reached the required cumulative counter value for
         the current working set percentage, simply accumulate this histogram
         entry into the running sums and continue to the next histogram
         entry.  */
      if (cum + histo_bucket->cum_value < working_set_cum_values[ws_ix])
        {
          cum += histo_bucket->cum_value;
          count += histo_bucket->num_counters;
          continue;
        }

      /* If adding the current histogram entry's cumulative counter value
         causes us to exceed the current working set size, then estimate
         how many of this histogram entry's counter values are required to
         reach the working set size, and fill in working set entries
         as we reach their expected cumulative value.  */
      for (c_num = 0, tmp_cum = cum;
           c_num < histo_bucket->num_counters && ws_ix < NUM_GCOV_WORKING_SETS;
           c_num++)
        {
          count++;
          /* If we haven't reached the last histogram entry counter, add
             in the minimum value again. This will underestimate the
             cumulative sum so far, because many of the counter values in this
             entry may have been larger than the minimum. We could add in the
             average value every time, but that would require an expensive
             divide operation.  */
          if (c_num + 1 < histo_bucket->num_counters)
            tmp_cum += histo_bucket->min_value;
          /* If we have reached the last histogram entry counter, then add
             in the entire cumulative value.  */
          else
            tmp_cum = cum + histo_bucket->cum_value;

	  /* Next walk through successive working set entries and fill in
	     the statistics for any whose size we have reached by accumulating
	     this histogram counter.  */
	  while (ws_ix < NUM_GCOV_WORKING_SETS
		 && tmp_cum >= working_set_cum_values[ws_ix])
            {
              gcov_working_sets[ws_ix].num_counters = count;
              gcov_working_sets[ws_ix].min_counter
                  = histo_bucket->min_value;
              ws_ix++;
            }
        }
      /* Finally, update the running cumulative value since we were
         using a temporary above.  */
      cum += histo_bucket->cum_value;
    }
  gcov_nonruntime_assert (ws_ix == NUM_GCOV_WORKING_SETS);
}
#endif /* IN_GCOV <= 0 && !IN_LIBGCOV */
