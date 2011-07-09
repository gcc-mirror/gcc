/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2" } */

double
convert (long long in)
{
  double f;
  __builtin_memcpy( &f, &in, sizeof( in ) );
  return f;
}

/* { dg-final { scan-assembler-not "movapd" } } */
