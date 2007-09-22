/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test structure passing and return values involving fixed-point types.
   Based on the test from ../dfp/.  */

extern void abort (void);

struct example
{
  short _Fract sf;
  char dummy1;
  _Fract f;
  char dummy2;
  long _Fract lf;
  char dummy3;
  long long _Fract llf;
  char dummy4;
  unsigned short _Fract usf;
  char dummy5;
  unsigned _Fract uf;
  char dummy6;
  unsigned long _Fract ulf;
  char dummy7;
  unsigned long long _Fract ullf;
  char dummy8;
  _Sat short _Fract Ssf;
  char dummy9;
  _Sat _Fract Sf;
  char dummy10;
  _Sat long _Fract Slf;
  char dummy11;
  _Sat long long _Fract Sllf;
  char dummy12;
  _Sat unsigned short _Fract Susf;
  char dummy13;
  _Sat unsigned _Fract Suf;
  char dummy14;
  _Sat unsigned long _Fract Sulf;
  char dummy15;
  _Sat unsigned long long _Fract Sullf;
  char dummy16;
  short _Accum sa;
  char dummya1;
  _Accum a;
  char dummya2;
  long _Accum la;
  char dummya3;
  long long _Accum lla;
  char dummya4;
  unsigned short _Accum usa;
  char dummya5;
  unsigned _Accum ua;
  char dummya6;
  unsigned long _Accum ula;
  char dummya7;
  unsigned long long _Accum ulla;
  char dummya8;
  _Sat short _Accum Ssa;
  char dummya9;
  _Sat _Accum Sa;
  char dummya10;
  _Sat long _Accum Sla;
  char dummya11;
  _Sat long long _Accum Slla;
  char dummya12;
  _Sat unsigned short _Accum Susa;
  char dummya13;
  _Sat unsigned _Accum Sua;
  char dummya14;
  _Sat unsigned long _Accum Sula;
  char dummya15;
  _Sat unsigned long long _Accum Sulla;
  char dummya16;
} nums = { 0.1hr, 'a',
	   0.2r, 'b',
	   0.3lr, 'c',
	   0.4llr, 'd',
	   0.5uhr, 'e',
	   0.6ur, 'f',
	   0.7ulr, 'g',
	   0.8ullr, 'h',
	   0.11r, 'i',
	   0.22r, 'j',
	   0.33lr, 'k',
	   0.44llr, 'l',
	   0.55uhr, 'm',
	   0.66ur, 'n',
	   0.77ulr, 'o',
	   0.88ullr, 'p',
	   0.1hk, 'q',
	   0.2k, 'r',
	   0.3lk, 's',
	   0.4llk, 't',
	   0.5uhk, 'u',
	   0.6uk, 'v',
	   0.7ulk, 'w',
	   0.8ullk, 'x',
	   0.11k, 'y',
	   0.22k, 'z',
	   0.33lk, '0',
	   0.44llk, '1',
	   0.55uhk, '2',
	   0.66uk, '3',
	   0.77ulk, '4',
	   0.88ullk, '5'
	 };

/* A handful of functions that return the Nth argument of
   an incoming array.  */

#define FUNC(TYPE,NAME) \
TYPE NAME ## _func (struct example s) \
  { return s. NAME; }

FUNC (short _Fract, sf)
FUNC (_Fract, f)
FUNC (long _Fract, lf)
FUNC (long long _Fract, llf)
FUNC (unsigned short _Fract, usf)
FUNC (unsigned _Fract, uf)
FUNC (unsigned long _Fract, ulf)
FUNC (unsigned long long _Fract, ullf)
FUNC (_Sat short _Fract, Ssf)
FUNC (_Sat _Fract, Sf)
FUNC (_Sat long _Fract, Slf)
FUNC (_Sat long long _Fract, Sllf)
FUNC (_Sat unsigned short _Fract, Susf)
FUNC (_Sat unsigned _Fract, Suf)
FUNC (_Sat unsigned long _Fract, Sulf)
FUNC (_Sat unsigned long long _Fract, Sullf)
FUNC (short _Accum, sa)
FUNC (_Accum, a)
FUNC (long _Accum, la)
FUNC (long long _Accum, lla)
FUNC (unsigned short _Accum, usa)
FUNC (unsigned _Accum, ua)
FUNC (unsigned long _Accum, ula)
FUNC (unsigned long long _Accum, ulla)
FUNC (_Sat short _Accum, Ssa)
FUNC (_Sat _Accum, Sa)
FUNC (_Sat long _Accum, Sla)
FUNC (_Sat long long _Accum, Slla)
FUNC (_Sat unsigned short _Accum, Susa)
FUNC (_Sat unsigned _Accum, Sua)
FUNC (_Sat unsigned long _Accum, Sula)
FUNC (_Sat unsigned long long _Accum, Sulla)

int main()
{
#define TEST(TYPE,NAME,VALUE) \
  { \
    if (NAME ## _func (nums) != VALUE) abort (); \
  }

  TEST (short _Fract, sf, 0.1hr)
  TEST (_Fract, f, 0.2r)
  TEST (long _Fract, lf, 0.3lr)
  TEST (long long _Fract, llf, 0.4llr)
  TEST (unsigned short _Fract, usf, 0.5uhr)
  TEST (unsigned _Fract, uf, 0.6ur)
  TEST (unsigned long _Fract, ulf, 0.7ulr)
  TEST (unsigned long long _Fract, ullf, 0.8ullr)
  TEST (_Sat short _Fract, Ssf, 0.11hr)
  TEST (_Sat _Fract, Sf, 0.22r)
  TEST (_Sat long _Fract, Slf, 0.33lr)
  TEST (_Sat long long _Fract, Sllf, 0.44llr)
  TEST (_Sat unsigned short _Fract, Susf, 0.55uhr)
  TEST (_Sat unsigned _Fract, Suf, 0.66ur)
  TEST (_Sat unsigned long _Fract, Sulf, 0.77ulr)
  TEST (_Sat unsigned long long _Fract, Sullf, 0.88ullr)
  TEST (short _Accum, sa, 0.1hk)
  TEST (_Accum, a, 0.2k)
  TEST (long _Accum, la, 0.3lk)
  TEST (long long _Accum, lla, 0.4llk)
  TEST (unsigned short _Accum, usa, 0.5uhk)
  TEST (unsigned _Accum, ua, 0.6uk)
  TEST (unsigned long _Accum, ula, 0.7ulk)
  TEST (unsigned long long _Accum, ulla, 0.8ullk)
  TEST (_Sat short _Accum, Ssa, 0.11hk)
  TEST (_Sat _Accum, Sa, 0.22k)
  TEST (_Sat long _Accum, Sla, 0.33lk)
  TEST (_Sat long long _Accum, Slla, 0.44llk)
  TEST (_Sat unsigned short _Accum, Susa, 0.55uhk)
  TEST (_Sat unsigned _Accum, Sua, 0.66uk)
  TEST (_Sat unsigned long _Accum, Sula, 0.77ulk)
  TEST (_Sat unsigned long long _Accum, Sulla, 0.88ullk)

  return 0;
}
