/* { dg-do compile } */
/* { dg-options "-O -march=rv32gcv -mabi=ilp32d" } */

typedef __attribute__((__vector_size__(16))) float V;

union {
  long double d;
  V v;
} u;

float f;
char c;

void
foo()
{
  u.v -= f;
  c *= u.d -= 0;
}
