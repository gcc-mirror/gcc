/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -Ofast" } */

__bf16 b;
int x;

void
foo ()
{
  if (x < b)
    x = 0;
}
