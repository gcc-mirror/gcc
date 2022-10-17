/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O -Wall -Wno-unused -ftrack-macro-expansion=0 -Wno-array-parameter" } */

/* C99 6.2.7: Compatible type and composite type.  */

#define FIXED_POINT_COMPOSITE_DECL(TYPE,NAME) \
  TYPE g1_##NAME(); \
  TYPE g2_##NAME(); \
  TYPE (*h1_##NAME)[2]; \
  TYPE (*h2_##NAME)[3]; \
  TYPE (*h3_##NAME)[4]; \
  TYPE f1_##NAME(TYPE(*)()); \
  TYPE f1_##NAME(TYPE(*)(TYPE*)); \
  TYPE f1_##NAME (TYPE(*g)(TYPE*)) \
   { \
     TYPE NAME; \
     NAME = ((TYPE (*) (TYPE*)) g)(&NAME); \
     NAME = ((TYPE (*) ()) g); \
     return NAME; \
   } \
  TYPE f2_##NAME(TYPE(*)[]); \
  TYPE f2_##NAME(TYPE(*)[3]);

#define FIXED_POINT_COMPOSITE_TEST(TYPE, NAME) \
do \
{ \
 TYPE NAME; \
 NAME = f1_##NAME(g1_##NAME); \
 NAME = f1_##NAME(g2_##NAME); \
 NAME = f2_##NAME(h1_##NAME); \
 NAME = f2_##NAME(h2_##NAME); \
 NAME = f2_##NAME(h3_##NAME); \
} while(0)

FIXED_POINT_COMPOSITE_DECL(short _Fract, sf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Fract, f);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(long _Fract, lf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(long long _Fract, llf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned short _Fract, usf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned _Fract, uf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned long _Fract, ulf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned long long _Fract, ullf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat short _Fract, Ssf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat _Fract, Sf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat long _Fract, Slf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat long long _Fract, Sllf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned short _Fract, Susf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned _Fract, Suf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned long _Fract, Sulf);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned long long _Fract, Sullf);  /* { dg-error "incompatible types when assigning" } */

FIXED_POINT_COMPOSITE_DECL(short _Accum, sk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Accum, k);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(long _Accum, lk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(long long _Accum, llk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned short _Accum, usk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned _Accum, uk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned long _Accum, ulk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(unsigned long long _Accum, ullk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat short _Accum, Ssk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat _Accum, Sk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat long _Accum, Slk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat long long _Accum, Sllk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned short _Accum, Susk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned _Accum, Suk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned long _Accum, Sulk);  /* { dg-error "incompatible types when assigning" } */
FIXED_POINT_COMPOSITE_DECL(_Sat unsigned long long _Accum, Sullk);  /* { dg-error "incompatible types when assigning" } */

int main()
{
  FIXED_POINT_COMPOSITE_TEST(short _Fract, sf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Fract, f);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(long _Fract, lf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(long long _Fract, llf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned short _Fract, usf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned _Fract, uf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned long _Fract, ulf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned long long _Fract, ullf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat short _Fract, Ssf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat _Fract, Sf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat long _Fract, Slf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat long long _Fract, Sllf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned short _Fract, Susf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned _Fract, Suf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned long _Fract, Sulf);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned long long _Fract, Sullf);  /* { dg-warning "incompatible pointer type" } */

  FIXED_POINT_COMPOSITE_TEST(short _Accum, sk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Accum, k);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(long _Accum, lk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(long long _Accum, llk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned short _Accum, usk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned _Accum, uk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned long _Accum, ulk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(unsigned long long _Accum, ullk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat short _Accum, Ssk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat _Accum, Sk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat long _Accum, Slk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat long long _Accum, Sllk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned short _Accum, Susk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned _Accum, Suk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned long _Accum, Sulk);  /* { dg-warning "incompatible pointer type" } */
  FIXED_POINT_COMPOSITE_TEST(_Sat unsigned long long _Accum, Sullk);  /* { dg-warning "incompatible pointer type" } */

  return 0;
}

/* Match all extra informative notes.  */
/* { dg-message "note: expected '\[^\n'\]*' but argument is of type '\[^\n'\]*'" "note: expected" { target *-*-* } 0 } */
