// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_FNV_HASH_H
#define RUST_FNV_HASH_H

namespace Rust {
namespace Hash {

const uint64_t offset128Lower = 0x62b821756295c58d;
const uint64_t offset128Higher = 0x6c62272e07bb0142;
const uint64_t prime128Lower = 0x13b;
const uint64_t prime128Shift = 24;

// ported from https://github.com/golang/go/blob/master/src/hash/fnv/fnv.go
class FNV128
{
public:
  FNV128 () { reset (); }

  void reset ()
  {
    buf[0] = offset128Higher;
    buf[1] = offset128Lower;
  }

  void write (const unsigned char *in, size_t len)
  {
    for (size_t i = 0; i < len; i++)
      {
	unsigned char c = in[i];

	// https://stackoverflow.com/questions/28868367/getting-the-high-part-of-64-bit-integer-multiplication
	uint64_t a = prime128Lower;
	uint64_t b = buf[1];

	uint64_t a_lo = (uint32_t) a;
	uint64_t a_hi = a >> 32;
	uint64_t b_lo = (uint32_t) b;
	uint64_t b_hi = b >> 32;

	uint64_t a_x_b_hi = a_hi * b_hi;
	uint64_t a_x_b_mid = a_hi * b_lo;
	uint64_t b_x_a_mid = b_hi * a_lo;
	uint64_t a_x_b_lo = a_lo * b_lo;

	uint64_t carry_bit
	  = ((uint64_t) (uint32_t) a_x_b_mid + (uint64_t) (uint32_t) b_x_a_mid
	     + (a_x_b_lo >> 32))
	    >> 32;

	uint64_t multhi
	  = a_x_b_hi + (a_x_b_mid >> 32) + (b_x_a_mid >> 32) + carry_bit;

	uint64_t s0 = multhi;		      // high
	uint64_t s1 = prime128Lower * buf[1]; // low

	s0 += buf[1] << (prime128Shift + prime128Lower * buf[0]);

	// Update the values
	buf[1] = s1;
	buf[0] = s0;
	buf[1] ^= (uint64_t) c;
      }
  }

  void sum (uint64_t *hi, uint64_t *lo) const
  {
    *hi = buf[0];
    *lo = buf[1];
  }

private:
  uint64_t buf[2];
};

} // namespace Hash
} // namespace Rust

#endif // RUST_FNV_HASH_H
