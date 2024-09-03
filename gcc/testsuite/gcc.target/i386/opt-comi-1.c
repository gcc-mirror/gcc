/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=sse -msse2" } */
/* { dg-final { scan-assembler-times "comiss" 9 } } */
/* { dg-final { scan-assembler-times "set" 9 } } */

int is_ordered_and_nonequal_sh_1 (float a, float b)
{
  return !__builtin_isunordered (a, b) && (a != b);
}

int is_ordered_and_nonequal_sh_2 (float a, float b)
{
  return !__builtin_isunordered (a, b) && (b != a);
}

int is_ordered_and_nonequal_sh_3 (float a, float b)
{
  return (b != a) && !__builtin_isunordered (a, b);
}

int is_ordered_and_nonequal_sh_4 (float a, float b)
{
  return !__builtin_isunordered (a, b) && !(a == b);
}

int is_ordered_and_nonequal_sh_5 (float a, float b)
{
  return !__builtin_isunordered (a, b) && !(b == a);
}

int is_ordered_and_nonequal_sh_6 (float a, float b)
{
  return !(b == a) && !__builtin_isunordered (a, b);
}

int is_ordered_or_nonequal_sh_7 (float a, float b)
{
  return !(__builtin_isunordered (a, b) || (a == b));
}

int is_ordered_or_nonequal_sh_8 (float a, float b)
{
  return !(__builtin_isunordered (a, b) || (b == a));
}

int is_ordered_or_nonequal_sh_9 (float a, float b)
{
  return !((a == b) || __builtin_isunordered (b, a));
}
