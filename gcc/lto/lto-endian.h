/* Very simple endian-ness layer for LTO object file handling
   Copyright 2010 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This header file provides a simple way to handle object files in
   another endian-ness than the host machine.  This is necesarry to
   enable cross-compilation with LTO enabled.  Targets that use the
   ELF binary object format do not need this (libelf already handles
   endian-ness) but for COFF and Mach-O the functions in this header
   are used in the minimal binary object reader/writer.
   
   For all functions in this header, the user is responsible for
   making sure that the memory accesses are valid.  */

#ifndef GCC_LTO_ENDIAN_H
#define GCC_LTO_ENDIAN_H

#include <stdint.h>
#include <inttypes.h>

static inline uint16_t
get_uint16_le (const unsigned char *ptr)
{
  return ptr[0] | (ptr[1] << 8);
}

static inline uint32_t
get_uint32_le (const unsigned char *ptr)
{
  return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24);
}

static inline uint64_t
get_uint64_le (const unsigned char *ptr_)
{
#define ptr (uint64_t) ptr_
  return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24)
         | (ptr[4] << 32) | (ptr[5] << 40) | (ptr[6] << 48) | (ptr[7] << 56);
#undef ptr
}

static inline uint16_t
get_uint16_be (const unsigned char *ptr)
{
  return ptr[1] | (ptr[2] << 8);
}

static inline uint32_t
get_uint32_be (const unsigned char *ptr)
{
  return ptr[3] | (ptr[2] << 8) | (ptr[1] << 16) | (ptr[0] << 24);
}

static inline uint64_t
get_uint64_be (const unsigned char *ptr_)
{
#define ptr (uint64_t) ptr_
  return ptr[7] | (ptr[6] << 8) | (ptr[5] << 16) | (ptr[4] << 24)
         | (ptr[3] << 32) | (ptr[2] << 40) | (ptr[1] << 48) | (ptr[0] << 56);
#undef ptr
}

static inline void
put_uint16_le (unsigned char *ptr, uint16_t data)
{
  ptr[0] = data & 0xff;
  ptr[1] = (data >> 8) & 0xff;
}

static inline void
put_uint32_le (unsigned char *ptr, uint32_t data)
{
  ptr[0] = data & 0xff;
  ptr[1] = (data >> 8) & 0xff;
  ptr[2] = (data >> 16) & 0xff;
  ptr[3] = (data >> 24) & 0xff;
}

static inline void
put_uint64_le (unsigned char *ptr, uint64_t data)
{
  ptr[0] = data & 0xff;
  ptr[1] = (data >> 8) & 0xff;
  ptr[2] = (data >> 16) & 0xff;
  ptr[3] = (data >> 24) & 0xff;
  ptr[4] = (data >> 32) & 0xff;
  ptr[5] = (data >> 40) & 0xff;
  ptr[6] = (data >> 48) & 0xff;
  ptr[7] = (data >> 56) & 0xff;
}

static inline void
put_uint16_be (unsigned char *ptr, uint16_t data)
{
  ptr[1] = data & 0xff;
  ptr[0] = (data >> 8) & 0xff;
}

static inline void
put_uint32_be (unsigned char *ptr, uint32_t data)
{
  ptr[3] = data & 0xff;
  ptr[2] = (data >> 8) & 0xff;
  ptr[1] = (data >> 16) & 0xff;
  ptr[0] = (data >> 24) & 0xff;
}

static inline void
put_uint64_be (unsigned char *ptr, uint64_t data)
{
  ptr[7] = data & 0xff;
  ptr[6] = (data >> 8) & 0xff;
  ptr[5] = (data >> 16) & 0xff;
  ptr[4] = (data >> 24) & 0xff;
  ptr[3] = (data >> 32) & 0xff;
  ptr[2] = (data >> 40) & 0xff;
  ptr[1] = (data >> 48) & 0xff;
  ptr[0] = (data >> 56) & 0xff;
}

static inline void
get_string (unsigned char *ptr, char *dest, size_t len)
{
  memcpy (dest, ptr, len);
}

static inline void
put_string (unsigned char *ptr, char *src, size_t len)
{
  memcpy (ptr, src, len);
}

/* Use the target macro BYTES_BIG_ENDIAN to choose.  */

static inline uint16_t
get_uint16 (const unsigned char *ptr)
{
  if (BYTES_BIG_ENDIAN)
    return get_uint16_be (ptr);
  else
    return get_uint16_le (ptr);
}

static inline uint32_t
get_uint32 (const unsigned char *ptr)
{
  if (BYTES_BIG_ENDIAN)
    return get_uint32_be (ptr);
  else
    return get_uint32_le (ptr);
}

static inline uint64_t
get_uint64 (const unsigned char *ptr)
{
  if (BYTES_BIG_ENDIAN)
    return get_uint64_be (ptr);
  else
    return get_uint64_le (ptr);
}

static inline void
put_uint16 (unsigned char *ptr, uint16_t data)
{
  if (BYTES_BIG_ENDIAN)
    put_uint16_be (ptr, data);
  else
    put_uint16_le (ptr, data);
}

static inline void
put_uint32 (unsigned char *ptr, uint32_t data)
{
  if (BYTES_BIG_ENDIAN)
    put_uint32_be (ptr, data);
  else
    put_uint32_le (ptr, data);
}

static inline void
put_uint64 (unsigned char *ptr, uint64_t data)
{
  if (BYTES_BIG_ENDIAN)
    put_uint64_be (ptr, data);
  else
    put_uint64_le (ptr, data);
}

#endif /* GCC_LTO_ENDIAN_H  */

