/* Verify OpenACC 'firstprivate' mappings for C++ reference types.  */

/* This file is also sourced from
   '../../../../libgomp/testsuite/libgomp.oacc-c++/firstprivate-mappings-1.C'
   as an execution test.  */

/* See also '../../c-c++-common/goacc/firstprivate-mappings-1.c'.  */

/* { dg-additional-options "-fdump-tree-omplower" } */

/* { dg-additional-options "-fext-numeric-literals" { target c++ } } */

#include <stdbool.h>
#include <stdint.h>
#include <string.h>


#ifdef __SIZEOF_INT128__
# define HAVE_INT128 1
#else
# define HAVE_INT128 0
#endif


/* The one is only relevant for offloading compilation; will always be enabled
   when doing tree scanning.  */
#ifdef ACC_DEVICE_TYPE_nvidia
/* PR71064.  */
# define DO_LONG_DOUBLE 0
#else
# define DO_LONG_DOUBLE 1
#endif


/* Simplify scanning for function names in tree dumps.  */
#ifdef __cplusplus
extern "C" {
#endif


/* Inside the following OpenACC 'parallel' constructs' regions, we modify the
   'firstprivate' variables, so that we can check that we don't copy these
   back.  */


static void
p (short *&spi)
{
  short *spo;
#pragma acc parallel \
  copyout (spo) \
  firstprivate (spi)
  {
    spo = ++spi;
  }
  if (spo != spi + 1)
    __builtin_abort ();
}


static void
b (bool &bi)
{
  bool bo;
#pragma acc parallel \
  copyout (bo) \
  firstprivate (bi)
  {
    bo = (bi = !bi);
  }
  if (bo != !bi)
    __builtin_abort ();
}


static void
i (int8_t &i8i,
   uint8_t &u8i,
   int16_t &i16i,
   uint16_t &u16i,
   int32_t &i32i,
   uint32_t &u32i,
   int64_t &i64i,
   uint64_t &u64i)
{
  int8_t i8o;
  uint8_t u8o;
  int16_t i16o;
  uint16_t u16o;
  int32_t i32o;
  uint32_t u32o;
  int64_t i64o;
  uint64_t u64o;
#pragma acc parallel \
  copyout (i8o) \
  firstprivate (i8i) \
  copyout (u8o) \
  firstprivate (u8i) \
  copyout (i16o) \
  firstprivate (i16i) \
  copyout (u16o) \
  firstprivate (u16i) \
  copyout (i32o) \
  firstprivate (i32i) \
  copyout (u32o) \
  firstprivate (u32i) \
  copyout (i64o) \
  firstprivate (i64i) \
  copyout (u64o) \
  firstprivate (u64i)
  {
    i8o = --i8i;
    u8o = ++u8i;
    i16o = --i16i;
    u16o = ++u16i;
    i32o = --i32i;
    u32o = ++u32i;
    i64o = --i64i;
    u64o = ++u64i;
  }
  if (i8o != i8i - 1)
    __builtin_abort ();
  if (u8o != u8i + 1)
    __builtin_abort ();
  if (i16o != i16i - 1)
    __builtin_abort ();
  if (u16o != u16i + 1)
    __builtin_abort ();
  if (i32o != i32i - 1)
    __builtin_abort ();
  if (u32o != u32i + 1)
    __builtin_abort ();
  if (i64o != i64i - 1)
    __builtin_abort ();
  if (u64o != u64i + 1)
    __builtin_abort ();
}


#if HAVE_INT128
static void
i128 (__int128 &i128i, unsigned __int128 &u128i)
{
  __int128 i128o;
  unsigned __int128 u128o;
# pragma acc parallel \
  copyout (i128o) \
  firstprivate (i128i) \
  copyout(u128o) \
  firstprivate (u128i)
  {
    i128o = --i128i;
    u128o = ++u128i;
  }
  if (i128o != i128i - 1)
    __builtin_abort ();
  if (u128o != u128i + 1)
    __builtin_abort ();
}
#endif


static void
flt_dbl (float &flti, double &dbli)
{
  float flto;
  double dblo;
#pragma acc parallel \
  copyout (flto) \
  firstprivate (flti) \
  copyout (dblo) \
  firstprivate (dbli)
  {
    flto = --flti;
    dblo = --dbli;
  }
  if (flto != flti - 1)
    __builtin_abort ();
  if (dblo != dbli - 1)
    __builtin_abort ();
}


static void
ldbl (long double &ldbli)
{
#if DO_LONG_DOUBLE
  long double ldblo;
# pragma acc parallel \
  copyout (ldblo) \
  firstprivate (ldbli)
  {
    ldblo = --ldbli;
  }
  if (ldblo != ldbli - 1)
    __builtin_abort ();
#endif
}


static void
c (_Complex unsigned char &cuci,
   _Complex signed short &cssi,
   _Complex unsigned int &cuii,
   _Complex signed long &csli,
   _Complex float &cflti,
   _Complex double &cdbli)
{
  _Complex unsigned char cuco;
  _Complex signed short csso;
  _Complex unsigned int cuio;
  _Complex signed long cslo;
  _Complex float cflto;
  _Complex double cdblo;
#pragma acc parallel \
  copyout (cuco) \
  firstprivate (cuci) \
  copyout (csso) \
  firstprivate (cssi) \
  copyout (cuio) \
  firstprivate (cuii) \
  copyout (cslo) \
  firstprivate (csli) \
  copyout (cflto) \
  firstprivate (cflti) \
  copyout (cdblo) \
  firstprivate (cdbli)
  {
    cuco = (cuci += (1 + 1j));
    csso = (cssi -= (1 + 1j));
    cuio = (cuii += (1 + 1j));
    cslo = (csli -= (1 + 1j));
    cflto = (cflti -= (1 + 1j));
    cdblo = (cdbli -= (1 + 1j));
  }
  if (cuco != cuci + (1 + 1j))
    __builtin_abort ();
  if (csso != cssi - (1 + 1j))
    __builtin_abort ();
  if (cuio != cuii + (1 + 1j))
    __builtin_abort ();
  if (cslo != csli - (1 + 1j))
    __builtin_abort ();
  if (cflto != cflti - (1 + 1j))
    __builtin_abort ();
  if (cdblo != cdbli - (1 + 1j))
    __builtin_abort ();
}


static void
cldbl (_Complex long double &cldbli)
{
#if DO_LONG_DOUBLE
  _Complex long double cldblo;
# pragma acc parallel \
  copyout (cldblo) \
  firstprivate (cldbli)
  {
    cldblo = (cldbli -= (1 + 1j));
  }
  if (cldblo != cldbli - (1 + 1j))
    __builtin_abort ();
#endif
}


#define V_EQ(v1, v2) \
  ({ \
    __typeof__ (v1) v_d = (v1) != (v2); \
    __typeof__ (v_d) v_0 = { 0 }; \
    memcmp (&v_d, &v_0, sizeof v_d) == 0; \
  })

typedef uint8_t __attribute__ ((vector_size (2 * sizeof (uint8_t)))) v2u8;
typedef int16_t __attribute__ ((vector_size (4 * sizeof (int16_t)))) v4i16;
typedef uint32_t __attribute__ ((vector_size (8 * sizeof (uint32_t)))) v8u32;
typedef int64_t __attribute__ ((vector_size (16 * sizeof (int64_t)))) v16i64;
typedef float __attribute__ ((vector_size (1 * sizeof (float)))) v1flt;
typedef float __attribute__ ((vector_size (2 * sizeof (float)))) v2flt;
typedef float __attribute__ ((vector_size (4 * sizeof (float)))) v4flt;
typedef float __attribute__ ((vector_size (8 * sizeof (float)))) v8flt;
typedef double __attribute__ ((vector_size (1 * sizeof (double)))) v1dbl;
typedef double __attribute__ ((vector_size (2 * sizeof (double)))) v2dbl;
typedef double __attribute__ ((vector_size (4 * sizeof (double)))) v4dbl;
typedef double __attribute__ ((vector_size (8 * sizeof (double)))) v8dbl;

static void
v (v2u8 &v2u8i, v4i16 &v4i16i, v8u32 &v8u32i, v16i64 &v16i64i,
   v1flt &v1flti, v2flt &v2flti, v4flt &v4flti, v8flt &v8flti,
   v1dbl &v1dbli, v2dbl &v2dbli, v4dbl &v4dbli, v8dbl &v8dbli)
{
  v2u8 v2u8o;
  v4i16 v4i16o;
  v8u32 v8u32o;
  v16i64 v16i64o;
  v1flt v1flto;
  v2flt v2flto;
  v4flt v4flto;
  v8flt v8flto;
  v1dbl v1dblo;
  v2dbl v2dblo;
  v4dbl v4dblo;
  v8dbl v8dblo;
#pragma acc parallel \
  copyout (v2u8o) \
  firstprivate (v2u8i) \
  copyout (v4i16o) \
  firstprivate (v4i16i) \
  copyout (v8u32o) \
  firstprivate (v8u32i) \
  copyout (v16i64o) \
  firstprivate (v16i64i) \
  copyout (v1flto) \
  firstprivate (v1flti) \
  copyout (v2flto) \
  firstprivate (v2flti) \
  copyout (v4flto) \
  firstprivate (v4flti) \
  copyout (v8flto) \
  firstprivate (v8flti) \
  copyout (v1dblo) \
  firstprivate (v1dbli) \
  copyout (v2dblo) \
  firstprivate (v2dbli) \
  copyout (v4dblo) \
  firstprivate (v4dbli) \
  copyout (v8dblo) \
  firstprivate (v8dbli)
  {
    v2u8o = ++v2u8i;
    v4i16o = --v4i16i;
    v8u32o = ++v8u32i;
    v16i64o = --v16i64i;
    v1flto = --v1flti;
    v2flto = --v2flti;
    v4flto = --v4flti;
    v8flto = --v8flti;
    v1dblo = --v1dbli;
    v2dblo = --v2dbli;
    v4dblo = --v4dbli;
    v8dblo = --v8dbli;
  }
  if (!V_EQ (v2u8o, v2u8i + 1))
    __builtin_abort ();
  if (!V_EQ (v4i16o, v4i16i - 1))
    __builtin_abort ();
  if (!V_EQ (v8u32o, v8u32i + 1))
    __builtin_abort ();
  if (!V_EQ (v16i64o, v16i64i - 1))
    __builtin_abort ();
  if (!V_EQ (v1flto, v1flti - 1))
    __builtin_abort ();
  if (!V_EQ (v2flto, v2flti - 1))
    __builtin_abort ();
  if (!V_EQ (v4flto, v4flti - 1))
    __builtin_abort ();
  if (!V_EQ (v8flto, v8flti - 1))
    __builtin_abort ();
  if (!V_EQ (v1dblo, v1dbli - 1))
    __builtin_abort ();
  if (!V_EQ (v2dblo, v2dbli - 1))
    __builtin_abort ();
  if (!V_EQ (v4dblo, v4dbli - 1))
    __builtin_abort ();
  if (!V_EQ (v8dblo, v8dbli - 1))
    __builtin_abort ();
}


/* "error: could not find an integer type of the same size as 'long double'" */
#if HAVE_INT128
typedef long double __attribute__ ((vector_size (1 * sizeof (long double)))) v1ldbl;
typedef long double __attribute__ ((vector_size (2 * sizeof (long double)))) v2ldbl;
typedef long double __attribute__ ((vector_size (4 * sizeof (long double)))) v4ldbl;
typedef long double __attribute__ ((vector_size (8 * sizeof (long double)))) v8ldbl;

static void
vldbl (v1ldbl &v1ldbli, v2ldbl &v2ldbli, v4ldbl &v4ldbli, v8ldbl &v8ldbli)
{
# if DO_LONG_DOUBLE
  v1ldbl v1ldblo;
  v2ldbl v2ldblo;
  v4ldbl v4ldblo;
  v8ldbl v8ldblo;
#  pragma acc parallel \
  copyout (v1ldblo) \
  firstprivate (v1ldbli) \
  copyout (v2ldblo) \
  firstprivate (v2ldbli) \
  copyout (v4ldblo) \
  firstprivate (v4ldbli) \
  copyout (v8ldblo) \
  firstprivate (v8ldbli)
  {
    v1ldblo = --v1ldbli;
    v2ldblo = --v2ldbli;
    v4ldblo = --v4ldbli;
    v8ldblo = --v8ldbli;
  }
  if (!V_EQ (v1ldblo, v1ldbli - 1))
    __builtin_abort ();
  if (!V_EQ (v2ldblo, v2ldbli - 1))
    __builtin_abort ();
  if (!V_EQ (v4ldblo, v4ldbli - 1))
    __builtin_abort ();
  if (!V_EQ (v8ldblo, v8ldbli - 1))
    __builtin_abort ();
# endif
}
#endif


static void
vla (int &array_li)
{
  _Complex double array[array_li];
  uint32_t array_so;
#pragma acc parallel \
  copyout (array_so)
  /* The gimplifier has created an implicit 'firstprivate' clause for the array
     length.
     { dg-final { scan-tree-dump {(?n)#pragma omp target oacc_parallel map\(from:array_so \[len: 4\]\) firstprivate\(} omplower } }
     (C++ computes an intermediate value, so can't scan for 'firstprivate(array_li)'.)  */
  {
    array_so = sizeof array;
  }
  if (array_so != sizeof array)
    __builtin_abort ();
}


#ifdef __cplusplus
}
#endif


int
main (int argc, char *argv[])
{
  {
    short s;
    short *sp = &s;
    p (sp);
  }

  {
    bool bi = true;
    b (bi);
  }

  {
    int8_t i8i = -1;
    uint8_t u8i = 1;
    int16_t i16i = -2;
    uint16_t u16i = 2;
    int32_t i32i = -3;
    uint32_t u32i = 3;
    int64_t i64i = -4;
    uint64_t u64i = 4;
    i (i8i, u8i, i16i, u16i, i32i, u32i, i64i, u64i);
  }

#if HAVE_INT128
  {
    __int128 i128i = -8;
    unsigned __int128 u128i = 8;
    i128 (i128i, u128i);
  }
#endif

  {
    float flti = .5;
    double dbli = .25;
    flt_dbl (flti, dbli);
  }

  {
    long double ldbli = .125;
    ldbl (ldbli);
  }

  {
    _Complex unsigned char cuci = 1 + 2j;
    _Complex signed short cssi = -2 + (-4j);
    _Complex unsigned int cuii = 3 + 6j;
    _Complex signed long csli = -4 + (-8j);
    _Complex float cflti = .5 + 1j;
    _Complex double cdbli = .25 + .5j;
    c (cuci, cssi, cuii, csli, cflti, cdbli);
  }

  {
    _Complex long double cldbli = .125 + .25j;
    cldbl (cldbli);
  }

  {
    v2u8 v2u8i = {2, 3};
    v4i16 v4i16i = { -1, -2, 5, 4 };
    v8u32 v8u32i = { 3, 6, 9, 11};
    v16i64 v16i64i = { 10, 21, -25, 44, 31, -1, 1, 222, -1, -12, 52, -44, -13, 1, -1, -222};
    v1flt v1flti = { -.5 };
    v2flt v2flti = { 1.5, -2.5 };
    v4flt v4flti = { 3.5, -4.5, -5.5, -6.5 };
    v8flt v8flti = { -7.5, 8.5, 9.5, 10.5, -11.5, -12.5, 13.5, 14.5 };
    v1dbl v1dbli = { 0.25 };
    v2dbl v2dbli = { -1.25, -2.25 };
    v4dbl v4dbli = { 3.25, -4.25, 5.25, 6.25 };
    v8dbl v8dbli = { 7.25, 8.25, -9.25, -10.25, -11.25, 12.25, 13.25, -14.25 };
    v (v2u8i, v4i16i, v8u32i, v16i64i,
       v1flti, v2flti, v4flti, v8flti,
       v1dbli, v2dbli, v4dbli, v8dbli);
  }

#if HAVE_INT128
  {
    v1ldbl v1ldbli = { -0.125 };
    v2ldbl v2ldbli = { 1.125, -2.125 };
    v4ldbl v4ldbli = { -3.125, -4.125, 5.125, -6.125 };
    v8ldbl v8ldbli = { 7.125, -8.125, -9.125, 10.125, 11.125, 12.125, 13.125, 14.125 };
    vldbl (v1ldbli, v2ldbli, v4ldbli, v8ldbli);
  }
#endif

  vla (argc);

  return 0;
}
