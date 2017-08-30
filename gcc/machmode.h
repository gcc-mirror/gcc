/* Machine mode definitions for GCC; included by rtl.h and tree.h.
   Copyright (C) 1991-2017 Free Software Foundation, Inc.

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

#ifndef HAVE_MACHINE_MODES
#define HAVE_MACHINE_MODES

extern CONST_MODE_SIZE unsigned short mode_size[NUM_MACHINE_MODES];
extern const unsigned short mode_precision[NUM_MACHINE_MODES];
extern const unsigned char mode_inner[NUM_MACHINE_MODES];
extern const unsigned char mode_nunits[NUM_MACHINE_MODES];
extern CONST_MODE_UNIT_SIZE unsigned char mode_unit_size[NUM_MACHINE_MODES];
extern const unsigned short mode_unit_precision[NUM_MACHINE_MODES];
extern const unsigned char mode_wider[NUM_MACHINE_MODES];
extern const unsigned char mode_2xwider[NUM_MACHINE_MODES];

template<typename T>
struct mode_traits
{
  /* For use by the machmode support code only.

     There are cases in which the machmode support code needs to forcibly
     convert a machine_mode to a specific mode class T, and in which the
     context guarantees that this is valid without the need for an assert.
     This can be done using:

       return typename mode_traits<T>::from_int (mode);

     when returning a T and:

       res = T (typename mode_traits<T>::from_int (mode));

     when assigning to a value RES that must be assignment-compatible
     with (but possibly not the same as) T.  */
#ifdef USE_ENUM_MODES
  /* Allow direct conversion of enums to specific mode classes only
     when USE_ENUM_MODES is defined.  This is only intended for use
     by gencondmd, so that it can tell more easily when .md conditions
     are always false.  */
  typedef machine_mode from_int;
#else
  /* Here we use an enum type distinct from machine_mode but with the
     same range as machine_mode.  T should have a constructor that
     accepts this enum type; it should not have a constructor that
     accepts machine_mode.

     We use this somewhat indirect approach to avoid too many constructor
     calls when the compiler is built with -O0.  For example, even in
     unoptimized code, the return statement above would construct the
     returned T directly from the numerical value of MODE.  */
  enum from_int { dummy = MAX_MACHINE_MODE };
#endif
};

template<>
struct mode_traits<machine_mode>
{
  /* machine_mode itself needs no conversion.  */
  typedef machine_mode from_int;
};

/* Get the name of mode MODE as a string.  */

extern const char * const mode_name[NUM_MACHINE_MODES];
#define GET_MODE_NAME(MODE)  mode_name[MODE]

/* Mode classes.  */

#include "mode-classes.def"
#define DEF_MODE_CLASS(M) M
enum mode_class { MODE_CLASSES, MAX_MODE_CLASS };
#undef DEF_MODE_CLASS
#undef MODE_CLASSES

/* Get the general kind of object that mode MODE represents
   (integer, floating, complex, etc.)  */

extern const unsigned char mode_class[NUM_MACHINE_MODES];
#define GET_MODE_CLASS(MODE)  ((enum mode_class) mode_class[MODE])

/* Nonzero if MODE is an integral mode.  */
#define INTEGRAL_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_INT		\
   || GET_MODE_CLASS (MODE) == MODE_PARTIAL_INT \
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT \
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_INT)

/* Nonzero if MODE is a floating-point mode.  */
#define FLOAT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   || GET_MODE_CLASS (MODE) == MODE_DECIMAL_FLOAT \
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT \
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_FLOAT)

/* Nonzero if MODE is a complex mode.  */
#define COMPLEX_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT	\
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)

/* Nonzero if MODE is a vector mode.  */
#define VECTOR_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_VECTOR_INT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_FLOAT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_FRACT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_UFRACT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_ACCUM	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_UACCUM)

/* Nonzero if MODE is a scalar integral mode.  */
#define SCALAR_INT_MODE_P(MODE)			\
  (GET_MODE_CLASS (MODE) == MODE_INT		\
   || GET_MODE_CLASS (MODE) == MODE_PARTIAL_INT)

/* Nonzero if MODE is a scalar floating point mode.  */
#define SCALAR_FLOAT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT		\
   || GET_MODE_CLASS (MODE) == MODE_DECIMAL_FLOAT)

/* Nonzero if MODE is a decimal floating point mode.  */
#define DECIMAL_FLOAT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_DECIMAL_FLOAT)

/* Nonzero if MODE is a scalar fract mode.  */
#define SCALAR_FRACT_MODE_P(MODE)	\
  (GET_MODE_CLASS (MODE) == MODE_FRACT)

/* Nonzero if MODE is a scalar ufract mode.  */
#define SCALAR_UFRACT_MODE_P(MODE)	\
  (GET_MODE_CLASS (MODE) == MODE_UFRACT)

/* Nonzero if MODE is a scalar fract or ufract mode.  */
#define ALL_SCALAR_FRACT_MODE_P(MODE)	\
  (SCALAR_FRACT_MODE_P (MODE) || SCALAR_UFRACT_MODE_P (MODE))

/* Nonzero if MODE is a scalar accum mode.  */
#define SCALAR_ACCUM_MODE_P(MODE)	\
  (GET_MODE_CLASS (MODE) == MODE_ACCUM)

/* Nonzero if MODE is a scalar uaccum mode.  */
#define SCALAR_UACCUM_MODE_P(MODE)	\
  (GET_MODE_CLASS (MODE) == MODE_UACCUM)

/* Nonzero if MODE is a scalar accum or uaccum mode.  */
#define ALL_SCALAR_ACCUM_MODE_P(MODE)	\
  (SCALAR_ACCUM_MODE_P (MODE) || SCALAR_UACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar fract or accum mode.  */
#define SIGNED_SCALAR_FIXED_POINT_MODE_P(MODE)	\
  (SCALAR_FRACT_MODE_P (MODE) || SCALAR_ACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar ufract or uaccum mode.  */
#define UNSIGNED_SCALAR_FIXED_POINT_MODE_P(MODE)	\
  (SCALAR_UFRACT_MODE_P (MODE) || SCALAR_UACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar fract, ufract, accum or uaccum mode.  */
#define ALL_SCALAR_FIXED_POINT_MODE_P(MODE)	\
  (SIGNED_SCALAR_FIXED_POINT_MODE_P (MODE)	\
   || UNSIGNED_SCALAR_FIXED_POINT_MODE_P (MODE))

/* Nonzero if MODE is a scalar/vector fract mode.  */
#define FRACT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FRACT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_FRACT)

/* Nonzero if MODE is a scalar/vector ufract mode.  */
#define UFRACT_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_UFRACT	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_UFRACT)

/* Nonzero if MODE is a scalar/vector fract or ufract mode.  */
#define ALL_FRACT_MODE_P(MODE)		\
  (FRACT_MODE_P (MODE) || UFRACT_MODE_P (MODE))

/* Nonzero if MODE is a scalar/vector accum mode.  */
#define ACCUM_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_ACCUM	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_ACCUM)

/* Nonzero if MODE is a scalar/vector uaccum mode.  */
#define UACCUM_MODE_P(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_UACCUM	\
   || GET_MODE_CLASS (MODE) == MODE_VECTOR_UACCUM)

/* Nonzero if MODE is a scalar/vector accum or uaccum mode.  */
#define ALL_ACCUM_MODE_P(MODE)		\
  (ACCUM_MODE_P (MODE) || UACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar/vector fract or accum mode.  */
#define SIGNED_FIXED_POINT_MODE_P(MODE)		\
  (FRACT_MODE_P (MODE) || ACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar/vector ufract or uaccum mode.  */
#define UNSIGNED_FIXED_POINT_MODE_P(MODE)	\
  (UFRACT_MODE_P (MODE) || UACCUM_MODE_P (MODE))

/* Nonzero if MODE is a scalar/vector fract, ufract, accum or uaccum mode.  */
#define ALL_FIXED_POINT_MODE_P(MODE)		\
  (SIGNED_FIXED_POINT_MODE_P (MODE)		\
   || UNSIGNED_FIXED_POINT_MODE_P (MODE))

/* Nonzero if CLASS modes can be widened.  */
#define CLASS_HAS_WIDER_MODES_P(CLASS)         \
  (CLASS == MODE_INT                           \
   || CLASS == MODE_PARTIAL_INT                \
   || CLASS == MODE_FLOAT                      \
   || CLASS == MODE_DECIMAL_FLOAT              \
   || CLASS == MODE_COMPLEX_FLOAT              \
   || CLASS == MODE_FRACT                      \
   || CLASS == MODE_UFRACT                     \
   || CLASS == MODE_ACCUM                      \
   || CLASS == MODE_UACCUM)

#define POINTER_BOUNDS_MODE_P(MODE)      \
  (GET_MODE_CLASS (MODE) == MODE_POINTER_BOUNDS)

/* An optional T (i.e. a T or nothing), where T is some form of mode class.  */
template<typename T>
class opt_mode
{
public:
  enum from_int { dummy = MAX_MACHINE_MODE };

  ALWAYS_INLINE opt_mode () : m_mode (E_VOIDmode) {}
  ALWAYS_INLINE opt_mode (const T &m) : m_mode (m) {}
  ALWAYS_INLINE opt_mode (from_int m) : m_mode (machine_mode (m)) {}

  machine_mode else_void () const;
  machine_mode else_blk () const;
  T require () const;

  bool exists () const;
  template<typename U> bool exists (U *) const;

private:
  machine_mode m_mode;
};

/* If the object contains a T, return its enum value, otherwise return
   E_VOIDmode.  */

template<typename T>
ALWAYS_INLINE machine_mode
opt_mode<T>::else_void () const
{
  return m_mode;
}

/* If the T exists, return its enum value, otherwise return E_BLKmode.  */

template<typename T>
inline machine_mode
opt_mode<T>::else_blk () const
{
  return m_mode == E_VOIDmode ? E_BLKmode : m_mode;
}

/* Assert that the object contains a T and return it.  */

template<typename T>
inline T
opt_mode<T>::require () const
{
  gcc_checking_assert (m_mode != E_VOIDmode);
  return typename mode_traits<T>::from_int (m_mode);
}

/* Return true if the object contains a T rather than nothing.  */

template<typename T>
ALWAYS_INLINE bool
opt_mode<T>::exists () const
{
  return m_mode != E_VOIDmode;
}

/* Return true if the object contains a T, storing it in *MODE if so.  */

template<typename T>
template<typename U>
inline bool
opt_mode<T>::exists (U *mode) const
{
  if (m_mode != E_VOIDmode)
    {
      *mode = T (typename mode_traits<T>::from_int (m_mode));
      return true;
    }
  return false;
}

/* A POD version of mode class T.  */

template<typename T>
struct pod_mode
{
  typedef typename mode_traits<T>::from_int from_int;

  machine_mode m_mode;
  ALWAYS_INLINE operator machine_mode () const { return m_mode; }
  ALWAYS_INLINE operator T () const { return from_int (m_mode); }
  ALWAYS_INLINE pod_mode &operator = (const T &m) { m_mode = m; return *this; }
};

/* Return true if mode M has type T.  */

template<typename T>
inline bool
is_a (machine_mode m)
{
  return T::includes_p (m);
}

/* Assert that mode M has type T, and return it in that form.  */

template<typename T>
inline T
as_a (machine_mode m)
{
  gcc_checking_assert (T::includes_p (m));
  return typename mode_traits<T>::from_int (m);
}

/* Convert M to an opt_mode<T>.  */

template<typename T>
inline opt_mode<T>
dyn_cast (machine_mode m)
{
  if (T::includes_p (m))
    return T (typename mode_traits<T>::from_int (m));
  return opt_mode<T> ();
}

/* Return true if mode M has type T, storing it as a T in *RESULT
   if so.  */

template<typename T, typename U>
inline bool
is_a (machine_mode m, U *result)
{
  if (T::includes_p (m))
    {
      *result = T (typename mode_traits<T>::from_int (m));
      return true;
    }
  return false;
}

/* Represents a machine mode that is known to be a SCALAR_INT_MODE_P.  */
class scalar_int_mode
{
public:
  typedef mode_traits<scalar_int_mode>::from_int from_int;

  ALWAYS_INLINE scalar_int_mode () {}
  ALWAYS_INLINE scalar_int_mode (from_int m) : m_mode (machine_mode (m)) {}
  ALWAYS_INLINE operator machine_mode () const { return m_mode; }

  static bool includes_p (machine_mode);

protected:
  machine_mode m_mode;
};

/* Return true if M is a scalar_int_mode.  */

inline bool
scalar_int_mode::includes_p (machine_mode m)
{
  return SCALAR_INT_MODE_P (m);
}

/* Represents a machine mode that is known to be a SCALAR_FLOAT_MODE_P.  */
class scalar_float_mode
{
public:
  typedef mode_traits<scalar_float_mode>::from_int from_int;

  ALWAYS_INLINE scalar_float_mode () {}
  ALWAYS_INLINE scalar_float_mode (from_int m) : m_mode (machine_mode (m)) {}
  ALWAYS_INLINE operator machine_mode () const { return m_mode; }

  static bool includes_p (machine_mode);

protected:
  machine_mode m_mode;
};

/* Return true if M is a scalar_float_mode.  */

inline bool
scalar_float_mode::includes_p (machine_mode m)
{
  return SCALAR_FLOAT_MODE_P (m);
}

/* Represents a machine mode that is known to be scalar.  */
class scalar_mode
{
public:
  typedef mode_traits<scalar_mode>::from_int from_int;

  ALWAYS_INLINE scalar_mode () {}
  ALWAYS_INLINE scalar_mode (from_int m) : m_mode (machine_mode (m)) {}
  ALWAYS_INLINE scalar_mode (const scalar_int_mode &m) : m_mode (m) {}
  ALWAYS_INLINE scalar_mode (const scalar_float_mode &m) : m_mode (m) {}
  ALWAYS_INLINE scalar_mode (const scalar_int_mode_pod &m) : m_mode (m) {}
  ALWAYS_INLINE operator machine_mode () const { return m_mode; }

  static bool includes_p (machine_mode);

protected:
  machine_mode m_mode;
};

/* Return true if M represents some kind of scalar value.  */

inline bool
scalar_mode::includes_p (machine_mode m)
{
  switch (GET_MODE_CLASS (m))
    {
    case MODE_INT:
    case MODE_PARTIAL_INT:
    case MODE_FRACT:
    case MODE_UFRACT:
    case MODE_ACCUM:
    case MODE_UACCUM:
    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
    case MODE_POINTER_BOUNDS:
      return true;
    default:
      return false;
    }
}

/* Return the base GET_MODE_SIZE value for MODE.  */

ALWAYS_INLINE unsigned short
mode_to_bytes (machine_mode mode)
{
#if GCC_VERSION >= 4001
  return (__builtin_constant_p (mode)
	  ? mode_size_inline (mode) : mode_size[mode]);
#else
  return mode_size[mode];
#endif
}

/* Return the base GET_MODE_BITSIZE value for MODE.  */

ALWAYS_INLINE unsigned short
mode_to_bits (machine_mode mode)
{
  return mode_to_bytes (mode) * BITS_PER_UNIT;
}

/* Return the base GET_MODE_PRECISION value for MODE.  */

ALWAYS_INLINE unsigned short
mode_to_precision (machine_mode mode)
{
  return mode_precision[mode];
}

/* Return the base GET_MODE_INNER value for MODE.  */

ALWAYS_INLINE scalar_mode
mode_to_inner (machine_mode mode)
{
#if GCC_VERSION >= 4001
  return scalar_mode::from_int (__builtin_constant_p (mode)
				? mode_inner_inline (mode)
				: mode_inner[mode]);
#else
  return scalar_mode::from_int (mode_inner[mode]);
#endif
}

/* Return the base GET_MODE_UNIT_SIZE value for MODE.  */

ALWAYS_INLINE unsigned char
mode_to_unit_size (machine_mode mode)
{
#if GCC_VERSION >= 4001
  return (__builtin_constant_p (mode)
	  ? mode_unit_size_inline (mode) : mode_unit_size[mode]);
#else
  return mode_unit_size[mode];
#endif
}

/* Return the base GET_MODE_UNIT_PRECISION value for MODE.  */

ALWAYS_INLINE unsigned short
mode_to_unit_precision (machine_mode mode)
{
#if GCC_VERSION >= 4001
  return (__builtin_constant_p (mode)
	  ? mode_unit_precision_inline (mode) : mode_unit_precision[mode]);
#else
  return mode_unit_precision[mode];
#endif
}

/* Return the base GET_MODE_NUNITS value for MODE.  */

ALWAYS_INLINE unsigned short
mode_to_nunits (machine_mode mode)
{
#if GCC_VERSION >= 4001
  return (__builtin_constant_p (mode)
	  ? mode_nunits_inline (mode) : mode_nunits[mode]);
#else
  return mode_nunits[mode];
#endif
}

/* Get the size in bytes of an object of mode MODE.  */

#define GET_MODE_SIZE(MODE) (mode_to_bytes (MODE))

/* Get the size in bits of an object of mode MODE.  */

#define GET_MODE_BITSIZE(MODE) (mode_to_bits (MODE))

/* Get the number of value bits of an object of mode MODE.  */

#define GET_MODE_PRECISION(MODE) (mode_to_precision (MODE))

/* Get the number of integral bits of an object of mode MODE.  */
extern CONST_MODE_IBIT unsigned char mode_ibit[NUM_MACHINE_MODES];
#define GET_MODE_IBIT(MODE) mode_ibit[MODE]

/* Get the number of fractional bits of an object of mode MODE.  */
extern CONST_MODE_FBIT unsigned char mode_fbit[NUM_MACHINE_MODES];
#define GET_MODE_FBIT(MODE) mode_fbit[MODE]

/* Get a bitmask containing 1 for all bits in a word
   that fit within mode MODE.  */

extern const unsigned HOST_WIDE_INT mode_mask_array[NUM_MACHINE_MODES];

#define GET_MODE_MASK(MODE) mode_mask_array[MODE]

/* Return the mode of the basic parts of MODE.  For vector modes this is the
   mode of the vector elements.  For complex modes it is the mode of the real
   and imaginary parts.  For other modes it is MODE itself.  */

#define GET_MODE_INNER(MODE) (mode_to_inner (MODE))

/* Get the size in bytes or bits of the basic parts of an
   object of mode MODE.  */

#define GET_MODE_UNIT_SIZE(MODE) mode_to_unit_size (MODE)

#define GET_MODE_UNIT_BITSIZE(MODE) \
  ((unsigned short) (GET_MODE_UNIT_SIZE (MODE) * BITS_PER_UNIT))

#define GET_MODE_UNIT_PRECISION(MODE) (mode_to_unit_precision (MODE))

/* Get the number of units in an object of mode MODE.  This is 2 for
   complex modes and the number of elements for vector modes.  */

#define GET_MODE_NUNITS(MODE) (mode_to_nunits (MODE))

/* Get the next wider natural mode (eg, QI -> HI -> SI -> DI -> TI).  */

template<typename T>
ALWAYS_INLINE opt_mode<T>
GET_MODE_WIDER_MODE (const T &m)
{
  return typename opt_mode<T>::from_int (mode_wider[m]);
}

/* For scalars, this is a mode with twice the precision.  For vectors,
   this is a mode with the same inner mode but with twice the elements.  */

template<typename T>
ALWAYS_INLINE opt_mode<T>
GET_MODE_2XWIDER_MODE (const T &m)
{
  return typename opt_mode<T>::from_int (mode_2xwider[m]);
}

/* Get the complex mode from the component mode.  */
extern const unsigned char mode_complex[NUM_MACHINE_MODES];
#define GET_MODE_COMPLEX_MODE(MODE) ((machine_mode) mode_complex[MODE])

/* Return the mode for data of a given size SIZE and mode class CLASS.
   If LIMIT is nonzero, then don't use modes bigger than MAX_FIXED_MODE_SIZE.
   The value is BLKmode if no other mode is found.  */

extern machine_mode mode_for_size (unsigned int, enum mode_class, int);

/* Return the machine mode to use for a MODE_INT of SIZE bits, if one
   exists.  If LIMIT is nonzero, modes wider than MAX_FIXED_MODE_SIZE
   will not be used.  */

inline opt_scalar_int_mode
int_mode_for_size (unsigned int size, int limit)
{
  return dyn_cast <scalar_int_mode> (mode_for_size (size, MODE_INT, limit));
}

/* Return the machine mode to use for a MODE_FLOAT of SIZE bits, if one
   exists.  */

inline opt_scalar_float_mode
float_mode_for_size (unsigned int size)
{
  return dyn_cast <scalar_float_mode> (mode_for_size (size, MODE_FLOAT, 0));
}

/* Similar to mode_for_size, but find the smallest mode for a given width.  */

extern machine_mode smallest_mode_for_size (unsigned int, enum mode_class);

/* Find the narrowest integer mode that contains at least SIZE bits.
   Such a mode must exist.  */

inline scalar_int_mode
smallest_int_mode_for_size (unsigned int size)
{
  return as_a <scalar_int_mode> (smallest_mode_for_size (size, MODE_INT));
}

/* Return an integer mode of exactly the same size as the input mode.  */

extern opt_scalar_int_mode int_mode_for_mode (machine_mode);

extern machine_mode bitwise_mode_for_mode (machine_mode);

/* Return a mode that is suitable for representing a vector,
   or BLKmode on failure.  */

extern machine_mode mode_for_vector (machine_mode, unsigned);

/* A class for iterating through possible bitfield modes.  */
class bit_field_mode_iterator
{
public:
  bit_field_mode_iterator (HOST_WIDE_INT, HOST_WIDE_INT,
			   HOST_WIDE_INT, HOST_WIDE_INT,
			   unsigned int, bool);
  bool next_mode (scalar_int_mode *);
  bool prefer_smaller_modes ();

private:
  opt_scalar_int_mode m_mode;
  /* We use signed values here because the bit position can be negative
     for invalid input such as gcc.dg/pr48335-8.c.  */
  HOST_WIDE_INT m_bitsize;
  HOST_WIDE_INT m_bitpos;
  HOST_WIDE_INT m_bitregion_start;
  HOST_WIDE_INT m_bitregion_end;
  unsigned int m_align;
  bool m_volatilep;
  int m_count;
};

/* Find the best mode to use to access a bit field.  */

extern bool get_best_mode (int, int, unsigned HOST_WIDE_INT,
			   unsigned HOST_WIDE_INT, unsigned int,
			   unsigned HOST_WIDE_INT, bool, scalar_int_mode *);

/* Determine alignment, 1<=result<=BIGGEST_ALIGNMENT.  */

extern CONST_MODE_BASE_ALIGN unsigned short mode_base_align[NUM_MACHINE_MODES];

extern unsigned get_mode_alignment (machine_mode);

#define GET_MODE_ALIGNMENT(MODE) get_mode_alignment (MODE)

/* For each class, get the narrowest mode in that class.  */

extern const unsigned char class_narrowest_mode[MAX_MODE_CLASS];
#define GET_CLASS_NARROWEST_MODE(CLASS) \
  ((machine_mode) class_narrowest_mode[CLASS])

/* The narrowest full integer mode available on the target.  */

#define NARROWEST_INT_MODE \
  (scalar_int_mode \
   (scalar_int_mode::from_int (class_narrowest_mode[MODE_INT])))

/* Return the narrowest mode in T's class.  */

template<typename T>
inline T
get_narrowest_mode (T mode)
{
  return typename mode_traits<T>::from_int
    (class_narrowest_mode[GET_MODE_CLASS (mode)]);
}

/* Define the integer modes whose sizes are BITS_PER_UNIT and BITS_PER_WORD
   and the mode whose class is Pmode and whose size is POINTER_SIZE.  */

extern scalar_int_mode byte_mode;
extern scalar_int_mode word_mode;
extern scalar_int_mode ptr_mode;

/* Target-dependent machine mode initialization - in insn-modes.c.  */
extern void init_adjust_machine_modes (void);

#define TRULY_NOOP_TRUNCATION_MODES_P(MODE1, MODE2) \
  TRULY_NOOP_TRUNCATION (GET_MODE_PRECISION (MODE1), \
			 GET_MODE_PRECISION (MODE2))

#define HWI_COMPUTABLE_MODE_P(MODE) \
  (SCALAR_INT_MODE_P (MODE) \
   && GET_MODE_PRECISION (MODE) <= HOST_BITS_PER_WIDE_INT)

struct int_n_data_t {
  /* These parts are initailized by genmodes output */
  unsigned int bitsize;
  scalar_int_mode_pod m;
  /* RID_* is RID_INTN_BASE + index into this array */
};

/* This is also in tree.h.  genmodes.c guarantees the're sorted from
   smallest bitsize to largest bitsize. */
extern bool int_n_enabled_p[NUM_INT_N_ENTS];
extern const int_n_data_t int_n_data[NUM_INT_N_ENTS];

/* Return true if MODE has class MODE_INT, storing it as a scalar_int_mode
   in *INT_MODE if so.  */

template<typename T>
inline bool
is_int_mode (machine_mode mode, T *int_mode)
{
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      *int_mode = scalar_int_mode (scalar_int_mode::from_int (mode));
      return true;
    }
  return false;
}

/* Return true if MODE has class MODE_FLOAT, storing it as a
   scalar_float_mode in *FLOAT_MODE if so.  */

template<typename T>
inline bool
is_float_mode (machine_mode mode, T *float_mode)
{
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      *float_mode = scalar_float_mode (scalar_float_mode::from_int (mode));
      return true;
    }
  return false;
}

namespace mode_iterator
{
  /* Start mode iterator *ITER at the first mode in class MCLASS, if any.  */

  template<typename T>
  inline void
  start (opt_mode<T> *iter, enum mode_class mclass)
  {
    if (GET_CLASS_NARROWEST_MODE (mclass) == E_VOIDmode)
      *iter = opt_mode<T> ();
    else
      *iter = as_a<T> (GET_CLASS_NARROWEST_MODE (mclass));
  }

  inline void
  start (machine_mode *iter, enum mode_class mclass)
  {
    *iter = GET_CLASS_NARROWEST_MODE (mclass);
  }

  /* Return true if mode iterator *ITER has not reached the end.  */

  template<typename T>
  inline bool
  iterate_p (opt_mode<T> *iter)
  {
    return iter->exists ();
  }

  inline bool
  iterate_p (machine_mode *iter)
  {
    return *iter != E_VOIDmode;
  }

  /* Set mode iterator *ITER to the next widest mode in the same class,
     if any.  */

  template<typename T>
  inline void
  get_wider (opt_mode<T> *iter)
  {
    *iter = GET_MODE_WIDER_MODE (iter->require ());
  }

  inline void
  get_wider (machine_mode *iter)
  {
    *iter = GET_MODE_WIDER_MODE (*iter).else_void ();
  }

  /* Set mode iterator *ITER to the next widest mode in the same class.
     Such a mode is known to exist.  */

  template<typename T>
  inline void
  get_known_wider (T *iter)
  {
    *iter = GET_MODE_WIDER_MODE (*iter).require ();
  }

  /* Set mode iterator *ITER to the mode that is two times wider than the
     current one, if such a mode exists.  */

  template<typename T>
  inline void
  get_2xwider (opt_mode<T> *iter)
  {
    *iter = GET_MODE_2XWIDER_MODE (iter->require ());
  }

  inline void
  get_2xwider (machine_mode *iter)
  {
    *iter = GET_MODE_2XWIDER_MODE (*iter).else_void ();
  }
}

/* Make ITERATOR iterate over all the modes in mode class CLASS,
   from narrowest to widest.  */
#define FOR_EACH_MODE_IN_CLASS(ITERATOR, CLASS)  \
  for (mode_iterator::start (&(ITERATOR), CLASS); \
       mode_iterator::iterate_p (&(ITERATOR)); \
       mode_iterator::get_wider (&(ITERATOR)))

/* Make ITERATOR iterate over all the modes in the range [START, END),
   in order of increasing width.  */
#define FOR_EACH_MODE(ITERATOR, START, END) \
  for ((ITERATOR) = (START); \
       (ITERATOR) != (END); \
       mode_iterator::get_known_wider (&(ITERATOR)))

/* Make ITERATOR iterate over START and all wider modes in the same
   class, in order of increasing width.  */
#define FOR_EACH_MODE_FROM(ITERATOR, START) \
  for ((ITERATOR) = (START); \
       mode_iterator::iterate_p (&(ITERATOR)); \
       mode_iterator::get_wider (&(ITERATOR)))

/* Make ITERATOR iterate over modes in the range [NARROWEST, END)
   in order of increasing width, where NARROWEST is the narrowest mode
   in END's class.  */
#define FOR_EACH_MODE_UNTIL(ITERATOR, END) \
  FOR_EACH_MODE (ITERATOR, get_narrowest_mode (END), END)

/* Make ITERATOR iterate over modes in the same class as MODE, in order
   of increasing width.  Start at the first mode wider than START,
   or don't iterate at all if there is no wider mode.  */
#define FOR_EACH_WIDER_MODE(ITERATOR, START) \
  for ((ITERATOR) = (START), mode_iterator::get_wider (&(ITERATOR)); \
       mode_iterator::iterate_p (&(ITERATOR)); \
       mode_iterator::get_wider (&(ITERATOR)))

/* Make ITERATOR iterate over modes in the same class as MODE, in order
   of increasing width, and with each mode being twice the width of the
   previous mode.  Start at the mode that is two times wider than START,
   or don't iterate at all if there is no such mode.  */
#define FOR_EACH_2XWIDER_MODE(ITERATOR, START) \
  for ((ITERATOR) = (START), mode_iterator::get_2xwider (&(ITERATOR)); \
       mode_iterator::iterate_p (&(ITERATOR)); \
       mode_iterator::get_2xwider (&(ITERATOR)))

#endif /* not HAVE_MACHINE_MODES */
