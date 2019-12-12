/* { dg-do compile } */
/* { dg-options "-march=zEC12 -fno-omit-frame-pointer -Os" } */


extern void j(int);

void
d(int e, long f, int g, int h, int i) {
  if (h == 5 && i >= 4 && i <= 7)
    h = e;
  j(h);
}
