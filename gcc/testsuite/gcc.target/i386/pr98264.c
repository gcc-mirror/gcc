/* { dg-do compile } */
/* { dg-additional-options "-O3 -w" } */

a[], g[], h[];
double b, c;
double j[];
i;
l() {
  int e = 0;
  for (; e < 80; e += 4) {
    a[0] = h[e];
    a[1] = h[e + 1];
    j[0] = a[0] - c;
    j[1] = a[1] + b;
    __attribute__(()) k = *(double *)a, e, f = e = 0;
    *g = k;
    double *d = j;
    for (; e < 2; e++)
      if (d[e])
        i = f;
  }
}
