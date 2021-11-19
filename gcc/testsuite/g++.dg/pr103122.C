/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned a;
int b;
short c;
void d(long) {
  for (bool e = (bool)c - 1; e < (bool)b - 1; e += 0)
    ;
  if (a) {
    for (char f = 0; f < 7; f = 7)
      for (int g = 0; g < c; g += 10)
        ;
    d(-!c);
  }
}
