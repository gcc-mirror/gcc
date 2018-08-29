/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O -mtune=athlon -msse4" } */

int i;
unsigned __attribute__ ((__vector_size__ (16))) v;

void
foo (void)
{
  v *= i;
  i = i > -(long long) v[0];
}
