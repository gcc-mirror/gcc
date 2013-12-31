/* { dg-do compile } */

float foo (float) __attribute__ ((target ("custom-fsqrts=128")));
float foo (float x)
{
  return __builtin_custom_fsqrts (x) + __builtin_custom_fnf (128, x);
}
