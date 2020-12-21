/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=128" } */

int a, b, c;

void foo(long e) {
  for (int f = 0; f < b; f ++)
    for (int g = 0; g < c; g ++)
      a = (short)e;
}
