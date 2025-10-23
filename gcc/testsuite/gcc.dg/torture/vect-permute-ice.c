/* { dg-do compile } */
/* This testcase triggered an ICE that was fixed by
   g:1ceda79ca5fe1a1a296624a98de8fd04958fbe55.  */
char *a;
char c, e;
_Bool f() {
  int g, d = 0;
  for (int h = 0; h < 128; h += 8) {
    char *b = &a[h];
    g = e * b[0] + c * b[1] + 2 * b[2] + 3 * b[3] + 4 * b[4] + 5 * b[5] +
        6 * b[6] + 7 * b[7];
    d += g;
  }
  return d;
}
