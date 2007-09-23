/* Test vector fixed-point instructions */
/* { dg-do compile { target {fixed_point} } } */
/* { dg-mips-options "-march=mips32r2 -mdspr2 -O2" } */
/* { dg-final { scan-assembler-times "\taddq_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsubq_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\taddu_s.qb\t" 1 } } */
/* { dg-final { scan-assembler-times "\taddu_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsubu_s.qb\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsubu_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\tmulq_rs.ph\t" 1 } } */

typedef _Sat unsigned short _Fract sat_v4uqq __attribute__ ((vector_size(4)));
typedef _Sat unsigned _Fract sat_v2uhq __attribute__ ((vector_size(4)));
typedef _Sat unsigned short _Accum sat_v2uha __attribute__ ((vector_size(4)));
typedef _Sat _Fract sat_v2hq __attribute__ ((vector_size(4)));
typedef _Sat short _Accum sat_v2ha __attribute__ ((vector_size(4)));

typedef unsigned short _Fract v4uqq __attribute__ ((vector_size(4)));
typedef unsigned _Fract v2uhq __attribute__ ((vector_size(4)));
typedef unsigned short _Accum v2uha __attribute__ ((vector_size(4)));
typedef _Fract v2hq __attribute__ ((vector_size(4)));
typedef short _Accum v2ha __attribute__ ((vector_size(4)));

NOMIPS16 sat_v2hq test1 (sat_v2hq a, sat_v2hq b)
{
  return a + b;
}

NOMIPS16 sat_v2ha test2 (sat_v2ha a, sat_v2ha b)
{
  return a + b;
}

NOMIPS16 sat_v2hq test3 (sat_v2hq a, sat_v2hq b)
{
  return a - b;
}

NOMIPS16 sat_v2ha test4 (sat_v2ha a, sat_v2ha b)
{
  return a - b;
}

NOMIPS16 sat_v4uqq test5 (sat_v4uqq a, sat_v4uqq b)
{
  return a + b;
}

NOMIPS16 sat_v2uhq test6 (sat_v2uhq a, sat_v2uhq b)
{
  return a + b;
}

NOMIPS16 sat_v2uha test7 (sat_v2uha a, sat_v2uha b)
{
  return a + b;
}

NOMIPS16 sat_v4uqq test8 (sat_v4uqq a, sat_v4uqq b)
{
  return a - b;
}

NOMIPS16 sat_v2uhq test9 (sat_v2uhq a, sat_v2uhq b)
{
  return a - b;
}

NOMIPS16 sat_v2uha test10 (sat_v2uha a, sat_v2uha b)
{
  return a - b;
}

NOMIPS16 sat_v2hq test11 (sat_v2hq a, sat_v2hq b)
{
  return a * b;
}

NOMIPS16 v2hq test12 (v2hq a, v2hq b)
{
  return a + b;
}

NOMIPS16 v2hq test13 (v2hq a, v2hq b)
{
  return a - b;
}

NOMIPS16 v2hq test14 (v2hq a, v2hq b)
{
  return a * b;
}

NOMIPS16 v2ha test15 (v2ha a, v2ha b)
{
  return a + b;
}

NOMIPS16 v2ha test16 (v2ha a, v2ha b)
{
  return a - b;
}

NOMIPS16 v4uqq test17 (v4uqq a, v4uqq b)
{
  return a + b;
}

NOMIPS16 v4uqq test18 (v4uqq a, v4uqq b)
{
  return a - b;
}

NOMIPS16 v2uhq test19 (v2uhq a, v2uhq b)
{
  return a + b;
}

NOMIPS16 v2uhq test20 (v2uhq a, v2uhq b)
{
  return a - b;
}

NOMIPS16 v2uha test21 (v2uha a, v2uha b)
{
  return a + b;
}

NOMIPS16 v2uha test22 (v2uha a, v2uha b)
{
  return a - b;
}
