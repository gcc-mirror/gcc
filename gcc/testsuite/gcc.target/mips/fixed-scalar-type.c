/* Test scalar fixed-point instructions */
/* { dg-do compile { target { fixed_point } } } */
/* { dg-options "-mdspr2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\taddu\t" 10 } } */
/* { dg-final { scan-assembler-times "\tsubu\t" 10 } } */
/* { dg-final { scan-assembler "\taddu_s.qb\t" } } */
/* { dg-final { scan-assembler-times "\taddu_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\taddq_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\taddq_s.w\t" 2 } } */
/* { dg-final { scan-assembler "\tsubu_s.qb\t" } } */
/* { dg-final { scan-assembler-times "\tsubu_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsubq_s.ph\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsubq_s.w\t" 2 } } */
/* { dg-final { scan-assembler-times "\tmulq_rs.ph\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmulq_rs.w\t" 1 } } */

short _Fract non_sat_test1 (short _Fract a, short _Fract b)
{
  return a + b;
}

_Fract non_sat_test2 (_Fract a, _Fract b)
{
  return a + b;
}

long _Fract non_sat_test3 (long _Fract a, long _Fract b)
{
  return a + b;
}

unsigned short _Fract non_sat_test4 (unsigned short _Fract a,
				     unsigned short _Fract b)
{
  return a + b;
}

unsigned _Fract non_sat_test5 (unsigned _Fract a, unsigned _Fract b)
{
  return a + b;
}

unsigned long _Fract non_sat_test6 (unsigned long _Fract a,
				    unsigned long _Fract b)
{
  return a + b;
}

short _Accum non_sat_test7 (short _Accum a, short _Accum b)
{
  return a + b;
}

_Accum non_sat_test8 (_Accum a, _Accum b)
{
  return a + b;
}

unsigned short _Accum non_sat_test9 (unsigned short _Accum a,
				     unsigned short _Accum b)
{
  return a + b;
}

unsigned _Accum non_sat_test10 (unsigned _Accum a, unsigned _Accum b)
{
  return a + b;
}

short _Fract non_sat_test11 (short _Fract a, short _Fract b)
{
  return a - b;
}

_Fract non_sat_test12 (_Fract a, _Fract b)
{
  return a - b;
}

long _Fract non_sat_test13 (long _Fract a, long _Fract b)
{
  return a - b;
}

unsigned short _Fract non_sat_test14 (unsigned short _Fract a,
				      unsigned short _Fract b)
{
  return a - b;
}

unsigned _Fract non_sat_test15 (unsigned _Fract a, unsigned _Fract b)
{
  return a - b;
}

unsigned long _Fract non_sat_test16 (unsigned long _Fract a,
				     unsigned long _Fract b)
{
  return a - b;
}

short _Accum non_sat_test17 (short _Accum a, short _Accum b)
{
  return a - b;
}

_Accum non_sat_test18 (_Accum a, _Accum b)
{
  return a - b;
}

unsigned short _Accum non_sat_test19 (unsigned short _Accum a,
				      unsigned short _Accum b)
{
  return a - b;
}

unsigned _Accum non_sat_test20 (unsigned _Accum a, unsigned _Accum b)
{
  return a - b;
}

NOMIPS16 _Sat unsigned short _Fract test1 (_Sat unsigned short _Fract a,
					   _Sat unsigned short _Fract b)
{
  return a + b;
}

NOMIPS16 _Sat unsigned _Fract test2 (_Sat unsigned  _Fract a,
				     _Sat unsigned _Fract b)
{
  return a + b;
}

NOMIPS16 _Sat unsigned short _Accum test3 (_Sat unsigned short _Accum a,
					   _Sat unsigned short _Accum b)
{
  return a + b;
}

NOMIPS16 _Sat _Fract test4 (_Sat _Fract a, _Sat _Fract b)
{
  return a + b;
}

NOMIPS16 _Sat long _Fract test5 (_Sat long _Fract a, _Sat long _Fract b)
{
  return a + b;
}

NOMIPS16 _Sat short _Accum test6 (_Sat short _Accum a, _Sat short _Accum b)
{
  return a + b;
}

NOMIPS16 _Sat _Accum test7 (_Sat _Accum a, _Sat _Accum b)
{
  return a + b;
}

NOMIPS16 _Sat unsigned short _Fract test8 (_Sat unsigned short _Fract a,
					   _Sat unsigned short _Fract b)
{
  return a - b;
}

NOMIPS16 _Sat unsigned _Fract test9 (_Sat unsigned  _Fract a,
				     _Sat unsigned _Fract b)
{
  return a - b;
}

NOMIPS16 _Sat unsigned short _Accum test10 (_Sat unsigned short _Accum a,
					    _Sat unsigned short _Accum b)
{
  return a - b;
}

NOMIPS16 _Sat _Fract test11 (_Sat _Fract a, _Sat _Fract b)
{
  return a - b;
}

NOMIPS16 _Sat long _Fract test12 (_Sat long _Fract a, _Sat long _Fract b)
{
  return a - b;
}

NOMIPS16 _Sat short _Accum test13 (_Sat short _Accum a, _Sat short _Accum b)
{
  return a - b;
}

NOMIPS16 _Sat _Accum test14 (_Sat _Accum a, _Sat _Accum b)
{
  return a - b;
}

NOMIPS16 _Sat _Fract test15 (_Sat _Fract a, _Sat _Fract b)
{
  return a * b;
}

NOMIPS16 _Sat long _Fract test16 (_Sat long _Fract a, _Sat long _Fract b)
{
  return a * b;
}

NOMIPS16 _Fract test17 (_Fract a, _Fract b)
{
  return a * b;
}

NOMIPS16 long _Fract test18 (long _Fract a, long _Fract b)
{
  return a * b;
}
