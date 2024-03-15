/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16" } */

long
foo(_Float16 f)
{
  return __builtin_lroundf16(f);
}
