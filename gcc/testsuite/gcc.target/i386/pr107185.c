/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

void
foo (float f)
{
  long p =  __builtin_lrintf (f);
}
