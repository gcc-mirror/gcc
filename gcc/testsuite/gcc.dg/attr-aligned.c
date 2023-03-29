/* PR c/87795 - Excessive alignment permitted for functions and labels
   { dg-do compile }
   { dg-options "-Wno-pedantic" } */

/* The maximum alignment GCC can handle.  */
#define ALIGN_MAX_HARD 0x10000000

/* Hardcode a few known values for testing the tight bounds.  */
#if __hpux__ && __hppa__ && __LP64__
   /* Maximum alignment for functions and objects with static storage
      duration that's expected to be accepted.  */
#  define ALIGN_MAX_STATIC      0x1000
   /* Excessive alignment for functions and objects with static storage
      duration that's expected to trigger an error.  */
#elif __APPLE__
# if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070
#   define ALIGN_MAX_STATIC      0x8000
# else
#   define ALIGN_MAX_STATIC      ALIGN_MAX_HARD
# endif
#elif __CRIS__
/* __BIGGEST_ALIGNMENT__ doesn't cover functions (16 bits for CRIS). */
#  define ALIGN_MAX_STATIC      512
#  define ALIGN_TOO_BIG_OFILE   (ALIGN_MAX_HARD << 1)
#elif pdp11
#  define ALIGN_MAX_STATIC      2
/* Work around a pdp11 ICE (see PR target/87821).  */
#  define ALIGN_MAX_AUTO        (ALIGN_MAX_HARD >> 14)
#elif __WIN32__ || __CYGWIN__
#  define ALIGN_MAX_STATIC      8192
#  define ALIGN_MAX_AUTO        8192
#elif __powerpc64__ || __x86_64__
/* Is this processor- or operating-system specific?  */
#  define ALIGN_MAX_STATIC      ALIGN_MAX_HARD
#else
   /* Guaranteed to be accepted regardless of the target for objects.
      This might not be true for alignment of functions though, so
      may need to be set to a target-specific value above.  */
#  define ALIGN_MAX_STATIC      __BIGGEST_ALIGNMENT__
   /* Guaranteed to be rejected regardless of the target.  */
#  define ALIGN_TOO_BIG_OFILE   (ALIGN_MAX_HARD << 1)
#endif

/* Maximum alignment for auto objects that's expected to be accepted.  */
#ifndef ALIGN_MAX_AUTO
#  define ALIGN_MAX_AUTO        ALIGN_MAX_HARD
#endif

#ifndef ALIGN_TOO_BIG_OFILE
#  define ALIGN_TOO_BIG_OFILE   (ALIGN_MAX_STATIC << 1)
#endif


#define ALIGN(N) __attribute__ ((aligned (N)))


/* Verify that types can be defined maximally overaligned using
   attribute aligned.  */
typedef ALIGN (ALIGN_MAX_HARD) char CharAlignedMaxHard;
typedef ALIGN (ALIGN_MAX_AUTO) char CharAlignedMaxAuto;
typedef ALIGN (ALIGN_MAX_STATIC) char CharAlignedMaxStatic;

#if ALIGN_TOO_BIG_OFILE < ALIGN_MAX_HARD
/* Also verify that an alignment greater than MAX_OFILE_ALIGNMENT
   is accepted unless the constant is as large as GCC's maximum
   supported alignment in any context.  */
typedef ALIGN (ALIGN_TOO_BIG_OFILE) char CharAlignedTooBig;
#endif

CharAlignedMaxStatic t_max;

/* Verify that globals can be defined maximally overaligned using
   attribute aligned.  */
ALIGN (ALIGN_MAX_STATIC) static const char aligned_sc_max = 0;
ALIGN (ALIGN_MAX_STATIC) const char aligned_c_max = aligned_sc_max;
ALIGN (ALIGN_MAX_STATIC) char aligned_v_max;
ALIGN (ALIGN_MAX_STATIC) void aligned_f_max (void);

_Static_assert (__alignof__ (aligned_sc_max) == ALIGN_MAX_STATIC);
_Static_assert (__alignof__ (aligned_c_max) == ALIGN_MAX_STATIC);
_Static_assert (__alignof__ (aligned_v_max) == ALIGN_MAX_STATIC);
_Static_assert (__alignof__ (aligned_f_max) == ALIGN_MAX_STATIC);


/* Verify that globals can be defined maximally overaligned using
   _Alignas.  */
_Alignas (ALIGN_MAX_STATIC) static const char alignas_sc_max = 0;
_Alignas (ALIGN_MAX_STATIC) const char alignas_c_max = alignas_sc_max;
_Alignas (ALIGN_MAX_STATIC) char alignas_v_max;

_Static_assert (__alignof__ (alignas_sc_max) == ALIGN_MAX_STATIC);
_Static_assert (__alignof__ (alignas_c_max) == ALIGN_MAX_STATIC);
_Static_assert (__alignof__ (alignas_v_max) == ALIGN_MAX_STATIC);


/* Verify that auto and static local variables can be defined maximally
   overaligned.  */

int accept_local_attribute_aligned (void)
{
#if ALIGN_TOO_BIG_OFILE < ALIGN_MAX_HARD
  /* Same as above.  */
  typedef ALIGN (ALIGN_TOO_BIG_OFILE) char LocalCharAlignedTooBig;
  LocalCharAlignedTooBig aligned_lt_too_big = 0;
  (void)&aligned_lt_too_big;
#endif

  static CharAlignedMaxStatic aligned_st_max;
  _Static_assert (_Alignof (aligned_st_max) == ALIGN_MAX_STATIC);

  CharAlignedMaxAuto aligned_t_max;
  _Static_assert (_Alignof (aligned_t_max) == ALIGN_MAX_AUTO);

  ALIGN (ALIGN_MAX_STATIC) char aligned_s_max;
  _Static_assert (_Alignof (aligned_s_max) == ALIGN_MAX_STATIC);

  ALIGN (ALIGN_MAX_AUTO) char aligned_l_max;
  _Static_assert (_Alignof (aligned_l_max) == ALIGN_MAX_AUTO);

  return aligned_st_max++ + aligned_t_max++ + aligned_s_max++ + aligned_l_max++;
}


int accept_local_alignas (void)
{
  _Alignas (ALIGN_MAX_STATIC) char alignas_s_max;
  _Static_assert (_Alignof (alignas_s_max) == ALIGN_MAX_STATIC);

  _Alignas (ALIGN_MAX_AUTO) char alignas_l_max;
  _Static_assert (_Alignof (alignas_l_max) == ALIGN_MAX_AUTO);

  return alignas_s_max++ + alignas_l_max++;
}


/* Verify that auto and static local variables are subject to the object
   file alignment limit.  The "object file" part may not be mentioned if
   the object file maximum is the same as GCC's internal maximum.  */

int reject_local_align (void)
{
  /* Ironically, the errors below are on different lines for each
     of the two declarations if the aligned attribute is on a line
     of its own.  */
  ALIGN (ALIGN_TOO_BIG_OFILE) static char aligned_sl_max;       /* { dg-error "requested alignment .\[0-9\]+. exceeds\( object file\)* maximum \[0-9\]+" } */

  _Alignas (ALIGN_TOO_BIG_OFILE) static char alignas_sl_max;    /* { dg-error "alignment" } */

  return aligned_sl_max++ + alignas_sl_max++;
}


/* Verify that global variables are subject to the object file
   alignment limit.  */

ALIGN (ALIGN_TOO_BIG_OFILE) char a_max_x_2;                    /* { dg-error "requested alignment .\[0-9\]+. exceeds\( object file\)* maximum \[0-9\]+" } */

_Alignas (ALIGN_TOO_BIG_OFILE) char a_max_x_2;                    /* { dg-error "alignment" } */

ALIGN (ALIGN_TOO_BIG_OFILE) void f_max_x_2 (void);             /* { dg-error "requested alignment .\[0-9\]+. exceeds\( object file\)* maximum \[0-9\]+" } */
