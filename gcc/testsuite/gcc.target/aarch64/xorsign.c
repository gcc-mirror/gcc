/* { dg-do compile } */
/* { dg-options "-O3" } */

double
check_d_pos (double x, double y)
{
  return x * __builtin_copysign (1.0, y);
}

float
check_f_pos (float x, float y)
{
  return x * __builtin_copysignf (1.0f, y);
}

long double
check_l_pos (long double x, long double y)
{
  return x * __builtin_copysignl (1.0, y);
}

/* --------------- */

double
check_d_neg (double x, double y)
{
  return x * __builtin_copysign (-1.0, y);
}

float
check_f_neg (float x, float y)
{
  return x * __builtin_copysignf (-1.0f, y);
}

long double
check_l_neg (long double x, long double y)
{
  return x * __builtin_copysignl (-1.0, y);
}

/* --------------- */

double
check_d_pos_rev (double x, double y)
{
  return __builtin_copysign (1.0, y) * x;
}

float
check_f_pos_rev (float x, float y)
{
  return __builtin_copysignf (1.0f, y) * x;
}

long double
check_l_pos_rev (long double x, long double y)
{
  return __builtin_copysignl (1.0, y) * x;
}

/* --------------- */

double
check_d_neg_rev (double x, double y)
{
  return __builtin_copysign (-1.0, y) * x;
}

float
check_f_neg_rev (float x, float y)
{
  return __builtin_copysignf (-1.0f, y) * x;
}

long double
check_l_neg_rev (long double x, long double y)
{
  return __builtin_copysignl (-1.0, y) * x;
}

/* { dg-final { scan-assembler "\[ \t\]?eor\[ \t\]?" } } */
/* { dg-final { scan-assembler "\[ \t\]?and\[ \t\]?" } } */
/* { dg-final { scan-assembler-not "copysign" } } */
/* { dg-final { scan-assembler-not "\[ \t\]?orr\[ \t\]?" } } */
/* { dg-final { scan-assembler-not "\[ \t\]?fmul\[ \t\]?" } } */
