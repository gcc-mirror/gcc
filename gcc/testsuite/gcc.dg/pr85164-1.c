/* { dg-options "-O2 -w" } */
a[];
b;
c() {
  unsigned long d;
  b = a[d - 1 >> 3];
}
