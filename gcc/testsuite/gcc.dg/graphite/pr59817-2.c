/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

void
xl(void)
{
  static int j3;
  for (j3 = 0; j3 < 1; ++j3) {
      static int f2;
      static int w7;
      short int b5;
      int ok;
      f2 = (b5 += ok) ? (w7 = 0): (w7 ? 0 : (f2 = ok));
  }
}
