/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

int a, b;

void d ()
{
  a << ~0 && b;
  b = a;
}
