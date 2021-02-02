/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=128" } */

int a, b;
short c;
void d(long e) {
  for (int f = 0; f < b; f += 1)
    for (short g = 0; g < c; g += 5)
      a = (short)e;
}
