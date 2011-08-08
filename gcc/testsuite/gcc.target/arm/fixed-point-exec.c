/* { dg-do run { target { fixed_point } } } */
/* { dg-options "-std=gnu99" } */

/* Check basic arithmetic ops for ARM fixed-point/saturating operation support.
   Not target-independent since we make various assumptions about precision and
   magnitudes of various types.  */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdfix.h>

#define TEST(TYPE, OP, NAME, SUFFIX)				\
  TYPE NAME##SUFFIX (TYPE A, TYPE B)				\
    {								\
      return A OP B;						\
    }

#define VARIANTS(TYPE, OP, NAME)				\
  TEST (short TYPE, OP, NAME, _short);				\
  TEST (TYPE, OP, NAME, _regular);				\
  TEST (long TYPE, OP, NAME, _long);				\
  TEST (_Sat short TYPE, OP, NAME, _sat_short);			\
  TEST (_Sat TYPE, OP, NAME, _sat_regular);			\
  TEST (_Sat long TYPE, OP, NAME, _sat_long);			\
  TEST (unsigned short TYPE, OP, NAME, _uns_short);		\
  TEST (unsigned TYPE, OP, NAME, _uns_regular);			\
  TEST (unsigned long TYPE, OP, NAME, _uns_long);		\
  TEST (unsigned _Sat short TYPE, OP, NAME, _uns_sat_short);	\
  TEST (unsigned _Sat TYPE, OP, NAME, _uns_sat_regular);	\
  TEST (unsigned _Sat long TYPE, OP, NAME, _uns_sat_long)

VARIANTS (_Fract, +, plus_fract);
VARIANTS (_Accum, +, plus_accum);
VARIANTS (_Fract, -, minus_fract);
VARIANTS (_Accum, -, minus_accum);
VARIANTS (_Fract, *, mult_fract);
VARIANTS (_Accum, *, mult_accum);
VARIANTS (_Accum, /, div_accum);

/* Inputs for signed add, multiply fractional tests.  */
short _Fract sf_a = 0.9hr;
short _Fract sf_b = -0.8hr;
_Fract f_a = 0.9r;
_Fract f_b = -0.8r;
long _Fract lf_a = 0.9lr;
long _Fract lf_b = -0.8lr;

/* Inputs for signed subtract fractional tests.  */
short _Fract sf_c = 0.7hr;
short _Fract sf_d = 0.9hr;
_Fract f_c = 0.7r;
_Fract f_d = 0.9r;
long _Fract lf_c = 0.7lr;
long _Fract lf_d = 0.9lr;

/* Inputs for unsigned add, subtract, multiply fractional tests.  */
unsigned short _Fract usf_a = 0.4uhr;
unsigned short _Fract usf_b = 0.3uhr;
unsigned _Fract uf_a = 0.4ur;
unsigned _Fract uf_b = 0.3ur;
unsigned long _Fract ulf_a = 0.4ulr;
unsigned long _Fract ulf_b = 0.3ulr;

/* Inputs for saturating signed add tests.  */
short _Sat _Fract sf_e = 0.8hr;
short _Sat _Fract sf_f = 0.8hr;
_Sat _Fract f_e = 0.8r;
_Sat _Fract f_f = 0.8r;
long _Sat _Fract lf_e = 0.8r;
long _Sat _Fract lf_f = 0.8r;

short _Sat _Fract sf_g = -0.8hr;
short _Sat _Fract sf_h = -0.8hr;
_Sat _Fract f_g = -0.8r;
_Sat _Fract f_h = -0.8r;
long _Sat _Fract lf_g = -0.8r;
long _Sat _Fract lf_h = -0.8r;

/* Inputs for saturating unsigned subtract tests.  */
unsigned short _Sat _Fract usf_c = 0.3uhr;
unsigned short _Sat _Fract usf_d = 0.4uhr;
unsigned _Sat _Fract uf_c = 0.3ur;
unsigned _Sat _Fract uf_d = 0.4ur;
unsigned long _Sat _Fract ulf_c = 0.3ulr;
unsigned long _Sat _Fract ulf_d = 0.4ulr;

/* Inputs for signed accumulator tests.  */

short _Accum sa_a = 1.25hk;
short _Accum sa_b = -1.5hk;
_Accum a_a = 100.25k;
_Accum a_b = -100.5k;
long _Accum la_a = 1000.25lk;
long _Accum la_b = -1000.5lk;

/* Inputs for unsigned accumulator tests.  */

unsigned short _Accum usa_a = 2.5uhk;
unsigned short _Accum usa_b = 1.75uhk;
unsigned _Accum ua_a = 255.5uk;
unsigned _Accum ua_b = 170.25uk;
unsigned long _Accum ula_a = 1550.5ulk;
unsigned long _Accum ula_b = 999.5ulk;

/* Inputs for signed saturating accumulator tests.  */

short _Sat _Accum sa_c = 240.0hk;
short _Sat _Accum sa_d = 250.0hk;
short _Sat _Accum sa_e = -240.0hk;
short _Sat _Accum sa_f = -250.0hk;
short _Sat _Accum sa_g = 0.5hk;

_Sat _Accum a_c = 65000.0k;
_Sat _Accum a_d = 20000.0k;
_Sat _Accum a_e = -65000.0k;
_Sat _Accum a_f = -20000.0k;
_Sat _Accum a_g = 0.5k;

long _Sat _Accum la_c = 3472883712.0lk;
long _Sat _Accum la_d = 3456106496.0lk;
long _Sat _Accum la_e = -3472883712.0lk;
long _Sat _Accum la_f = -3456106496.0lk;
long _Sat _Accum la_g = 0.5lk;

/* Inputs for unsigned saturating accumulator tests.  */

unsigned short _Sat _Accum usa_c = 250.0uhk;
unsigned short _Sat _Accum usa_d = 240.0uhk;
unsigned short _Sat _Accum usa_e = 0.5uhk;

unsigned _Sat _Accum ua_c = 65000.0uk;
unsigned _Sat _Accum ua_d = 20000.0uk;
unsigned _Sat _Accum ua_e = 0.5uk;

unsigned long _Sat _Accum ula_c = 3472883712.0ulk;
unsigned long _Sat _Accum ula_d = 3456106496.0ulk;
unsigned long _Sat _Accum ula_e = 0.5ulk;

#define CHECK(FN, EXP) do {						     \
  if (fabs ((float) (FN) - (EXP)) > 0.05)				     \
    {									     \
      fprintf (stderr, "result for " #FN " (as float): %f\n", (double) (FN));\
      abort ();								     \
    }									     \
  } while (0)

#define CHECK_EXACT(FN, EXP) do {					     \
  if ((FN) != (EXP))							     \
    {									     \
      fprintf (stderr, "result for " #FN " (as float): %f, should be %f\n",  \
	       (double) (FN), (double) (EXP));				     \
      abort ();								     \
    }									     \
  } while (0)

int
main (int argc, char *argv[])
{
  /* Fract/fract operations, non-saturating.  */

  CHECK (plus_fract_short (sf_a, sf_b), 0.1);
  CHECK (plus_fract_regular (f_a, f_b), 0.1);
  CHECK (plus_fract_long (lf_a, lf_b), 0.1);

  CHECK (plus_fract_uns_short (usf_a, usf_b), 0.7);
  CHECK (plus_fract_uns_regular (uf_a, uf_b), 0.7);
  CHECK (plus_fract_uns_long (ulf_a, ulf_b), 0.7);

  CHECK (minus_fract_short (sf_c, sf_d), -0.2);
  CHECK (minus_fract_regular (f_c, f_d), -0.2);
  CHECK (minus_fract_long (lf_c, lf_d), -0.2);

  CHECK (minus_fract_uns_short (usf_a, usf_b), 0.1);
  CHECK (minus_fract_uns_regular (uf_a, uf_b), 0.1);
  CHECK (minus_fract_uns_long (ulf_a, ulf_b), 0.1);

  CHECK (mult_fract_short (sf_a, sf_b), -0.72);
  CHECK (mult_fract_regular (f_a, f_b), -0.72);
  CHECK (mult_fract_long (lf_a, lf_b), -0.72);

  CHECK (mult_fract_uns_short (usf_a, usf_b), 0.12);
  CHECK (mult_fract_uns_regular (uf_a, uf_b), 0.12);
  CHECK (mult_fract_uns_long (ulf_a, ulf_b), 0.12);

  /* Fract/fract operations, saturating.  */

  CHECK (plus_fract_sat_short (sf_e, sf_f), 1.0);
  CHECK (plus_fract_sat_regular (f_e, f_f), 1.0);
  CHECK (plus_fract_sat_long (lf_e, lf_f), 1.0);

  CHECK (plus_fract_sat_short (sf_g, sf_h), -1.0);
  CHECK (plus_fract_sat_regular (f_g, f_h), -1.0);
  CHECK (plus_fract_sat_long (lf_g, lf_h), -1.0);
  
  CHECK (plus_fract_uns_sat_short (sf_e, sf_f), 1.0);
  CHECK (plus_fract_uns_sat_regular (f_e, f_f), 1.0);
  CHECK (plus_fract_uns_sat_long (lf_e, lf_f), 1.0);

  CHECK (plus_fract_sat_short (sf_a, sf_b), 0.1);
  CHECK (plus_fract_sat_regular (f_a, f_b), 0.1);
  CHECK (plus_fract_sat_long (lf_a, lf_b), 0.1);
  
  CHECK (plus_fract_uns_sat_short (usf_a, usf_b), 0.7);
  CHECK (plus_fract_uns_sat_regular (uf_a, uf_b), 0.7);
  CHECK (plus_fract_uns_sat_long (ulf_a, ulf_b), 0.7);

  CHECK (minus_fract_uns_sat_short (usf_c, usf_d), 0.0);
  CHECK (minus_fract_uns_sat_regular (uf_c, uf_d), 0.0);
  CHECK (minus_fract_uns_sat_short (ulf_c, ulf_d), 0.0);
  
  CHECK (minus_fract_sat_short (sf_c, sf_d), -0.2);
  CHECK (minus_fract_sat_regular (f_c, f_d), -0.2);
  CHECK (minus_fract_sat_long (lf_c, lf_d), -0.2);

  /* Accum/accum operations, non-saturating.  */

  CHECK (plus_accum_short (sa_a, sa_b), -0.25);
  CHECK (plus_accum_regular (a_a, a_b), -0.25);
  CHECK (plus_accum_long (la_a, la_b), -0.25);

  CHECK (minus_accum_short (sa_a, sa_b), 2.75);
  CHECK (minus_accum_regular (a_a, a_b), 200.75);
  CHECK (minus_accum_long (la_a, la_b), 2000.75);
  
  CHECK (mult_accum_short (sa_a, sa_b), -1.875);
  CHECK (mult_accum_regular (a_a, a_b), -10075.125);
  CHECK (mult_accum_long (la_a, la_b), -1000750.125);

  CHECK (div_accum_short (sa_a, sa_b), -1.25/1.5);
  CHECK (div_accum_regular (a_a, a_b), -100.25/100.5);
  CHECK (div_accum_long (la_a, la_b), -1000.25/1000.5);

  /* Unsigned accum/accum operations, non-saturating.  */
  
  CHECK (plus_accum_uns_short (usa_a, usa_b), 4.25);
  CHECK (plus_accum_uns_regular (ua_a, ua_b), 425.75);
  CHECK (plus_accum_uns_long (ula_a, ula_b), 2550.0);

  CHECK (minus_accum_uns_short (usa_a, usa_b), 0.75);
  CHECK (minus_accum_uns_regular (ua_a, ua_b), 85.25);
  CHECK (minus_accum_uns_long (ula_a, ula_b), 551.0);
  
  CHECK (mult_accum_uns_short (usa_a, usa_b), 4.375);
  CHECK (mult_accum_uns_regular (ua_a, ua_b), 43498.875);
  CHECK (mult_accum_uns_long (ula_a, ula_b), 1549724.75);

  CHECK (div_accum_uns_short (usa_a, usa_b), 2.5/1.75);
  CHECK (div_accum_uns_regular (ua_a, ua_b), 255.5/170.25);
  CHECK (div_accum_uns_long (ula_a, ula_b), 1550.5/999.5);

  /* Signed accum/accum operations, saturating.  */
  
  CHECK_EXACT (plus_accum_sat_short (sa_c, sa_d), SACCUM_MAX);
  CHECK_EXACT (plus_accum_sat_short (sa_e, sa_f), SACCUM_MIN);
  CHECK_EXACT (plus_accum_sat_regular (a_c, a_d), ACCUM_MAX);
  CHECK_EXACT (plus_accum_sat_regular (a_e, a_f), ACCUM_MIN);
  CHECK_EXACT (plus_accum_sat_long (la_c, la_d), LACCUM_MAX);
  CHECK_EXACT (plus_accum_sat_long (la_e, la_f), LACCUM_MIN);

  CHECK_EXACT (minus_accum_sat_short (sa_e, sa_d), SACCUM_MIN);
  CHECK_EXACT (minus_accum_sat_short (sa_c, sa_f), SACCUM_MAX);
  CHECK_EXACT (minus_accum_sat_regular (a_e, a_d), ACCUM_MIN);
  CHECK_EXACT (minus_accum_sat_regular (a_c, a_f), ACCUM_MAX);
  CHECK_EXACT (minus_accum_sat_long (la_e, la_d), LACCUM_MIN);
  CHECK_EXACT (minus_accum_sat_long (la_c, la_f), LACCUM_MAX);

  CHECK_EXACT (mult_accum_sat_short (sa_c, sa_d), SACCUM_MAX);
  CHECK_EXACT (mult_accum_sat_short (sa_c, sa_e), SACCUM_MIN);
  CHECK_EXACT (mult_accum_sat_regular (a_c, a_d), ACCUM_MAX);
  CHECK_EXACT (mult_accum_sat_regular (a_c, a_e), ACCUM_MIN);
  CHECK_EXACT (mult_accum_sat_long (la_c, la_d), LACCUM_MAX);
  CHECK_EXACT (mult_accum_sat_long (la_c, la_e), LACCUM_MIN);
  
  CHECK_EXACT (div_accum_sat_short (sa_d, sa_g), SACCUM_MAX);
  CHECK_EXACT (div_accum_sat_short (sa_e, sa_g), SACCUM_MIN);
  CHECK_EXACT (div_accum_sat_regular (a_c, a_g), ACCUM_MAX);
  CHECK_EXACT (div_accum_sat_regular (a_e, a_g), ACCUM_MIN);
  CHECK_EXACT (div_accum_sat_long (la_d, la_g), LACCUM_MAX);
  CHECK_EXACT (div_accum_sat_long (la_e, la_g), LACCUM_MIN);

  /* Unsigned accum/accum operations, saturating.  */

  CHECK_EXACT (plus_accum_uns_sat_short (usa_c, usa_d), USACCUM_MAX);
  CHECK_EXACT (plus_accum_uns_sat_regular (ua_c, ua_d), UACCUM_MAX);
  CHECK_EXACT (plus_accum_uns_sat_long (ula_c, ula_d), ULACCUM_MAX);
  
  CHECK_EXACT (minus_accum_uns_sat_short (usa_d, usa_c), 0uhk);
  CHECK_EXACT (minus_accum_uns_sat_regular (ua_d, ua_c), 0uk);
  CHECK_EXACT (minus_accum_uns_sat_long (ula_d, ula_c), 0ulk);

  CHECK_EXACT (mult_accum_uns_sat_short (usa_c, usa_d), USACCUM_MAX);
  CHECK_EXACT (mult_accum_uns_sat_regular (ua_c, ua_d), UACCUM_MAX);
  CHECK_EXACT (mult_accum_uns_sat_long (ula_c, ula_d), ULACCUM_MAX);

  CHECK_EXACT (div_accum_uns_sat_short (usa_c, usa_e), USACCUM_MAX);
  CHECK_EXACT (div_accum_uns_sat_regular (ua_c, ua_e), UACCUM_MAX);
  CHECK_EXACT (div_accum_uns_sat_long (ula_c, ula_e), ULACCUM_MAX);

  return 0;
}
