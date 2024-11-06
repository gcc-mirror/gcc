/* { dg-do compile } */
/* { dg-additional-options "-msse4" { target { x86_64-*-* i?86-*-* } } } */

int a, b;
_Complex long c;

void
foo ()
{
  do
    b = c || a;
  while (a);
}
