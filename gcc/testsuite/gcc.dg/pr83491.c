/* { dg-do compile } */
/* { dg-options "-O1 -funsafe-math-optimizations" } */

float a;
float b;
void bar ()
{
  a = __builtin_nanf ("");
  b = __builtin_powf (a, 2.5F);
}
