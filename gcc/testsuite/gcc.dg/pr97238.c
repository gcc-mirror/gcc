/* { dg-do compile } */
/* { dg-options "-O -Wno-psabi -w" } */

typedef int __attribute__ ((__vector_size__ (8))) V;
int b, c, e;
V d;

V
foo (void)
{
  return (b || e) | c > d | ((b || e) | c > d);
}
