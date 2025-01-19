/* Functions to support fixed-length bitmaps.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef GCC_BBITMAP_H
#define GCC_BBITMAP_H

/* Implementation of bounded (fixed length) bitmaps.

   This provides a drop-in replacement for bitmaps that have outgrown the
   storage capacity of a single integer.

   Sets are stored as a fixed length array of uint64_t elements.  The length of
   this array is given as a template parameter.  */

/* Use recusive templated functions to define constexpr operations.  */
template<int M>
struct bbitmap_operators
{
  /* Return a result that maps binary operator OP to elements [0, M) of
     X and Y, and takes the remaining elements from REST.  */
  template<typename Result, typename Operator, typename Arg, typename ...Rest>
  static constexpr Result binary(Operator op, const Arg &x, const Arg &y,
				 Rest ...rest)
  {
    return bbitmap_operators<M - 1>::template binary<Result, Operator, Arg>
      (op, x, y, op (x.val[M - 1], y.val[M - 1]), rest...);
  }

  /* Return a result that contains the bitwise inverse of elements [0, M) of X,
     and takes the remaining elements from REST.  */
  template<typename Result, typename Arg, typename ...Rest>
  static constexpr Result bit_not(const Arg &x, Rest ...rest)
  {
    return bbitmap_operators<M - 1>::template bit_not<Result, Arg>
      (x, ~(x.val[M - 1]), rest...);
  }

  /* Return true if any element [0, M) of X is nonzero.  */
  template<typename Arg>
  static constexpr bool non_zero(const Arg &x)
  {
    return (bool) x.val[M - 1]
      || bbitmap_operators<M - 1>::template non_zero<Arg> (x);
  }

  /* Return true if elements [0, M) of X are all equal to the corresponding
     elements of Y.  */
  template<typename Arg>
  static constexpr bool equal(const Arg &x, const Arg &y)
  {
    return x.val[M - 1] == y.val[M - 1]
      && bbitmap_operators<M - 1>::template equal<Arg> (x, y);
  }

  /* If bit index INDEX selects a bit in the first M elements, return a
     Result with that bit set and the other bits of the leading M elements
     clear.  Clear the leading M elements otherwise.  Take the remaining
     elements of the Result from REST.  */
  template<typename Result, typename ...Rest>
  static constexpr Result from_index(int index, Rest ...rest)
  {
    return bbitmap_operators<M - 1>::template from_index<Result>
      (index,
       uint64_t ((index - (M - 1) * 64) == (index & 63)) << (index & 63),
       rest...);
  }
};

/* These functions form the base for the recursive functions above.  They
   return either bitmap containing the elements passed in REST, or a default
   bool result.  */
template<>
struct bbitmap_operators<0>
{
  template<typename Result, typename Operator, typename Arg, typename ...Rest>
  static constexpr Result binary(Operator, const Arg, const Arg,
				 Rest ...rest)
  {
    return Result { rest... };
  }

  template<typename Result, typename Arg, typename ...Rest>
  static constexpr Result bit_not(const Arg, Rest ...rest)
  {
    return Result { rest... };
  }

  template<typename Arg>
  static constexpr bool non_zero(const Arg)
  {
    return false;
  }

  template<typename Arg>
  static constexpr bool equal(const Arg, const Arg)
  {
    return true;
  }

  template<typename Result, typename ...Rest>
  static constexpr Result from_index(int, Rest ...rest)
  {
    return Result { rest... };
  }
};

template<typename T>
constexpr T bbitmap_element_or(T x, T y) { return x | y;}

template<typename T>
constexpr T bbitmap_element_and(T x, T y) { return x & y;}

template<typename T>
constexpr T bbitmap_element_xor(T x, T y) { return x ^ y;}



template <int N>
class GTY((user)) bbitmap
{
public:
  uint64_t val[N];

  template<typename... Rest>
  constexpr bbitmap(Rest ...rest) : val{(uint64_t) rest...} {}

  constexpr bbitmap<N> operator|(const bbitmap<N> other) const
  {
    return bbitmap_operators<N>::template binary<bbitmap<N>>
      (bbitmap_element_or<uint64_t>, *this, other);
  }

  bbitmap<N> operator|=(const bbitmap<N> other)
  {
    for (int i = 0; i < N; i++)
      val[i] |= other.val[i];

    return this;
  }

  constexpr bbitmap<N> operator&(const bbitmap<N> other) const
  {
    return bbitmap_operators<N>::template binary<bbitmap<N>>
      (bbitmap_element_and<uint64_t>, *this, other);
  }

  bbitmap<N> operator&=(const bbitmap<N> other)
  {
    for (int i = 0; i < N; i++)
      val[i] &= other.val[i];

    return this;
  }

  constexpr bbitmap<N> operator^(const bbitmap<N> other) const
  {
    return bbitmap_operators<N>::template binary<bbitmap<N>>
      (bbitmap_element_xor<uint64_t>, *this, other);
  }

  bbitmap<N> operator^=(const bbitmap<N> other)
  {
    for (int i = 0; i < N; i++)
      val[i] ^= other.val[i];

    return this;
  }

  constexpr bbitmap<N> operator~() const
  {
    return bbitmap_operators<N>::template bit_not<bbitmap<N>>(*this);
  }

  constexpr bool operator!() const
  {
    return !(bbitmap_operators<N>::template non_zero<bbitmap<N>>(*this));
  }

  constexpr explicit operator bool() const
  {
    return bbitmap_operators<N>::template non_zero<bbitmap<N>>(*this);
  }

  constexpr bool operator==(const bbitmap<N> other) const
  {
    return bbitmap_operators<N>::template equal<bbitmap<N>>(*this, other);
  }

  constexpr bool operator!=(const bbitmap<N> other) const
  {
    return !(bbitmap_operators<N>::template equal<bbitmap<N>>(*this, other));
  }

  /* Return a bitmap with bit INDEX set and all other bits clear.  */

  static constexpr bbitmap<N> from_index (int index)
  {
    return bbitmap_operators<N>::template from_index<bbitmap<N>> (index);
  }
};

template<int N>
void
gt_ggc_mx (bbitmap<N> *)
{
}

template<int N>
void
gt_pch_nx (bbitmap<N> *)
{
}

template<int N>
void
gt_pch_nx (bbitmap<N> *, gt_pointer_operator, void *)
{
}

#endif
