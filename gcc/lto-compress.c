/* LTO IL compression streams.

   Copyright (C) 2009-2019 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "lto-streamer.h"
/* zlib.h includes other system headers.  Those headers may test feature
   test macros.  config.h may define feature test macros.  For this reason,
   zlib.h needs to be included after, rather than before, config.h and
   system.h.  */
#include <zlib.h>
#include "lto-compress.h"
#include "timevar.h"

#ifdef HAVE_ZSTD_H
#include <zstd.h>
#endif

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

/* Free the buffer and memory associated with STREAM.  */

static void
lto_destroy_compression_stream (struct lto_compression_stream *stream)
{
  free (stream->buffer);
  free (stream);
}

#ifdef HAVE_ZSTD_H
/* Return a zstd compression level that zstd will not reject.  Normalizes
   the compression level from the command line flag, clamping non-default
   values to the appropriate end of their valid range.  */

static int
lto_normalized_zstd_level (void)
{
  int level = flag_lto_compression_level;

  if (level < 0)
    level = 0;
  else if (level > ZSTD_maxCLevel ())
    level = ZSTD_maxCLevel ();

  return level;
}

/* Compress STREAM using ZSTD algorithm.  */

static void
lto_compression_zstd (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t size = stream->bytes;

  timevar_push (TV_IPA_LTO_COMPRESS);
  size_t const outbuf_length = ZSTD_compressBound (size);
  char *outbuf = (char *) xmalloc (outbuf_length);

  size_t const csize = ZSTD_compress (outbuf, outbuf_length, cursor, size,
				      lto_normalized_zstd_level ());

  if (ZSTD_isError (csize))
    internal_error ("compressed stream: %s", ZSTD_getErrorName (csize));

  stream->callback (outbuf, csize, NULL);

  lto_destroy_compression_stream (stream);
  free (outbuf);
  timevar_pop (TV_IPA_LTO_COMPRESS);
}

/* Uncompress STREAM using ZSTD algorithm.  */

static void
lto_uncompression_zstd (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t size = stream->bytes;

  timevar_push (TV_IPA_LTO_DECOMPRESS);
  unsigned long long const rsize = ZSTD_getFrameContentSize (cursor, size);
  if (rsize == ZSTD_CONTENTSIZE_ERROR)
    internal_error ("original not compressed with zstd");
  else if (rsize == ZSTD_CONTENTSIZE_UNKNOWN)
    internal_error ("original size unknown");

  char *outbuf = (char *) xmalloc (rsize);
  size_t const dsize = ZSTD_decompress (outbuf, rsize, cursor, size);

  if (ZSTD_isError (dsize))
    internal_error ("decompressed stream: %s", ZSTD_getErrorName (dsize));

  stream->callback (outbuf, dsize, stream->opaque);

  lto_destroy_compression_stream (stream);
  free (outbuf);
  timevar_pop (TV_IPA_LTO_DECOMPRESS);
}

#endif

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

static void ATTRIBUTE_UNUSED
lto_compression_zlib (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t remaining = stream->bytes;
  const size_t outbuf_length = Z_BUFFER_LENGTH;
  unsigned char *outbuf = (unsigned char *) xmalloc (outbuf_length);
  z_stream out_stream;
  size_t compressed_bytes = 0;
  int status;

  gcc_assert (stream->is_compression);

  timevar_push (TV_IPA_LTO_COMPRESS);

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
  timevar_pop (TV_IPA_LTO_COMPRESS);
}

void
lto_end_compression (struct lto_compression_stream *stream)
{
#ifdef HAVE_ZSTD_H
  lto_compression_zstd (stream);
#else
  lto_compression_zlib (stream);
#endif
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

static void
lto_uncompression_zlib (struct lto_compression_stream *stream)
{
  unsigned char *cursor = (unsigned char *) stream->buffer;
  size_t remaining = stream->bytes;
  const size_t outbuf_length = Z_BUFFER_LENGTH;
  unsigned char *outbuf = (unsigned char *) xmalloc (outbuf_length);
  size_t uncompressed_bytes = 0;

  gcc_assert (!stream->is_compression);
  timevar_push (TV_IPA_LTO_DECOMPRESS);

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
  timevar_pop (TV_IPA_LTO_DECOMPRESS);
}

void
lto_end_uncompression (struct lto_compression_stream *stream,
		       lto_compression compression)
{
#ifdef HAVE_ZSTD_H
  if (compression == ZSTD)
    {
      lto_uncompression_zstd (stream);
      return;
    }
#endif
  if (compression == ZSTD)
    internal_error ("compiler does not support ZSTD LTO compression");

  lto_uncompression_zlib (stream);
}
