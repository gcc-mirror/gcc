/* LTO IL compression streams.

   Copyright (C) 2009-2013 Free Software Foundation, Inc.
   Contributed by Simon Baldwin <simonb@google.com>

This file is part of GCC.

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

#include "config.h"
#include "system.h"
/* zlib.h includes other system headers.  Those headers may test feature
   test macros.  config.h may define feature test macros.  For this reason,
   zlib.h needs to be included after, rather than before, config.h and
   system.h.  */
#include <zlib.h>
#include "coretypes.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "langhooks.h"
#include "lto-streamer.h"
#include "lto-compress.h"

/* Compression stream structure, holds the flush callback and opaque token,
   the buffered data, and a note of whether compressing or uncompressing.  */

struct lto_compression_stream
{
  void (*callback) (const char *, unsigned, void *);
  void *opaque;
  char *buffer;
  size_t bytes;
  size_t allocation;
  bool is_compression;
};

/* Overall compression constants for zlib.  */

static const size_t Z_BUFFER_LENGTH = 4096;
static const size_t MIN_STREAM_ALLOCATION = 1024;

/* For zlib, allocate SIZE count of ITEMS and return the address, OPAQUE
   is unused.  */

static void *
lto_zalloc (void *opaque, unsigned items, unsigned size)
{
  gcc_assert (opaque == Z_NULL);
  return xmalloc (items * size);
}

/* For zlib, free memory at ADDRESS, OPAQUE is unused.  */

static void
lto_zfree (void *opaque, void *address)
{
  gcc_assert (opaque == Z_NULL);
  free (address);
}

/* Return a zlib compression level that zlib will not reject.  Normalizes
   the compression level from the command line flag, clamping non-default
   values to the appropriate end of their valid range.  */

static int
lto_normalized_zlib_level (void)
{
  int level = flag_lto_compression_level;

  if (level != Z_DEFAULT_COMPRESSION)
    {
      if (level < Z_NO_COMPRESSION)
	level = Z_NO_COMPRESSION;
      else if (level > Z_BEST_COMPRESSION)
	level = Z_BEST_COMPRESSION;
    }

  return level;
}

/* Create a new compression stream, with CALLBACK flush function passed
   OPAQUE token, IS_COMPRESSION indicates if compressing or uncompressing.  */

static struct lto_compression_stream *
lto_new_compression_stream (void (*callback) (const char *, unsigned, void *),
			    void *opaque, bool is_compression)
{
  struct lto_compression_stream *stream
    = (struct lto_compression_stream *) xmalloc (sizeof (*stream));

  memset (stream, 0, sizeof (*stream));
  stream->callback = callback;
  stream->opaque = opaque;
  stream->is_compression = is_compression;

  return stream;
}

/* Append NUM_CHARS from address BASE to STREAM.  */

static void
lto_append_to_compression_stream (struct lto_compression_stream *stream,
				  const char *base, size_t num_chars)
{
  size_t required = stream->bytes + num_chars;

  if (stream->allocation < required)
    {
      if (stream->allocation == 0)
	stream->allocation = MIN_STREAM_ALLOCATION;
      while (stream->allocation < required)
	stream->allocation *= 2;

      stream->buffer = (char *) xrealloc (stream->buffer, stream->allocation);
    }

  memcpy (stream->buffer + stream->bytes, base, num_chars);
  stream->bytes += num_chars;
}

/* Free the buffer and memory associated with STREAM.  */

static void
lto_destroy_compression_stream (struct lto_compression_stream *stream)
{
  free (stream->buffer);
  free (stream);
}

/* Return a new compression stream, with CALLBACK flush function passed
   OPAQUE token.  */

struct lto_compression_stream *
lto_start_compression (void (*callback) (const char *, unsigned, void *),
		       void *opaque)
{
  return lto_new_compression_stream (callback, opaque, true);
}

/* Append NUM_CHARS from address BASE to STREAM.  */

void
lto_compress_block (struct lto_compression_stream *stream,
		    const char *base, size_t num_chars)
{
  gcc_assert (stream->is_compression);

  lto_append_to_compression_stream (stream, base, num_chars);
  lto_stats.num_output_il_bytes += num_chars;
}

/* Finalize STREAM compression, and free stream allocations.  */

void
lto_end_compression (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t remaining = stream->bytes;
  const size_t outbuf_length = Z_BUFFER_LENGTH;
  unsigned char *outbuf = (unsigned char *) xmalloc (outbuf_length);
  z_stream out_stream;
  size_t compressed_bytes = 0;
  int status;

  gcc_assert (stream->is_compression);

  out_stream.next_out = outbuf;
  out_stream.avail_out = outbuf_length;
  out_stream.next_in = cursor;
  out_stream.avail_in = remaining;
  out_stream.zalloc = lto_zalloc;
  out_stream.zfree = lto_zfree;
  out_stream.opaque = Z_NULL;

  status = deflateInit (&out_stream, lto_normalized_zlib_level ());
  if (status != Z_OK)
    internal_error ("compressed stream: %s", zError (status));

  do
    {
      size_t in_bytes, out_bytes;

      status = deflate (&out_stream, Z_FINISH);
      if (status != Z_OK && status != Z_STREAM_END)
	internal_error ("compressed stream: %s", zError (status));

      in_bytes = remaining - out_stream.avail_in;
      out_bytes = outbuf_length - out_stream.avail_out;

      stream->callback ((const char *) outbuf, out_bytes, stream->opaque);
      lto_stats.num_compressed_il_bytes += out_bytes;
      compressed_bytes += out_bytes;

      cursor += in_bytes;
      remaining -= in_bytes;

      out_stream.next_out = outbuf;
      out_stream.avail_out = outbuf_length;
      out_stream.next_in = cursor;
      out_stream.avail_in = remaining;
    }
  while (status != Z_STREAM_END);

  status = deflateEnd (&out_stream);
  if (status != Z_OK)
    internal_error ("compressed stream: %s", zError (status));

  lto_destroy_compression_stream (stream);
  free (outbuf);
}

/* Return a new uncompression stream, with CALLBACK flush function passed
   OPAQUE token.  */

struct lto_compression_stream *
lto_start_uncompression (void (*callback) (const char *, unsigned, void *),
			 void *opaque)
{
  return lto_new_compression_stream (callback, opaque, false);
}

/* Append NUM_CHARS from address BASE to STREAM.  */

void
lto_uncompress_block (struct lto_compression_stream *stream,
		      const char *base, size_t num_chars)
{
  gcc_assert (!stream->is_compression);

  lto_append_to_compression_stream (stream, base, num_chars);
  lto_stats.num_input_il_bytes += num_chars;
}

/* Finalize STREAM uncompression, and free stream allocations.

   Because of the way LTO IL streams are compressed, there may be several
   concatenated compressed segments in the accumulated data, so for this
   function we iterate decompressions until no data remains.  */

void
lto_end_uncompression (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t remaining = stream->bytes;
  const size_t outbuf_length = Z_BUFFER_LENGTH;
  unsigned char *outbuf = (unsigned char *) xmalloc (outbuf_length);
  size_t uncompressed_bytes = 0;

  gcc_assert (!stream->is_compression);

  while (remaining > 0)
    {
      z_stream in_stream;
      size_t out_bytes;
      int status;

      in_stream.next_out = outbuf;
      in_stream.avail_out = outbuf_length;
      in_stream.next_in = cursor;
      in_stream.avail_in = remaining;
      in_stream.zalloc = lto_zalloc;
      in_stream.zfree = lto_zfree;
      in_stream.opaque = Z_NULL;

      status = inflateInit (&in_stream);
      if (status != Z_OK)
	internal_error ("compressed stream: %s", zError (status));

      do
	{
	  size_t in_bytes;

	  status = inflate (&in_stream, Z_SYNC_FLUSH);
	  if (status != Z_OK && status != Z_STREAM_END)
	    internal_error ("compressed stream: %s", zError (status));

	  in_bytes = remaining - in_stream.avail_in;
	  out_bytes = outbuf_length - in_stream.avail_out;

	  stream->callback ((const char *) outbuf, out_bytes, stream->opaque);
	  lto_stats.num_uncompressed_il_bytes += out_bytes;
	  uncompressed_bytes += out_bytes;

	  cursor += in_bytes;
	  remaining -= in_bytes;

	  in_stream.next_out = outbuf;
	  in_stream.avail_out = outbuf_length;
	  in_stream.next_in = cursor;
	  in_stream.avail_in = remaining;
	}
      while (!(status == Z_STREAM_END && out_bytes == 0));

      status = inflateEnd (&in_stream);
      if (status != Z_OK)
	internal_error ("compressed stream: %s", zError (status));
    }

  lto_destroy_compression_stream (stream);
  free (outbuf);
}
