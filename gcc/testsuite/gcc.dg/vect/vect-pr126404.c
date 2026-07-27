/* { dg-do compile } */

unsigned ff(int g7)
{
  int v3 = 8;
  int ob13;
  int ov14;
  do {
      ob13 = __builtin_add_overflow(2, g7, &ov14);
      g7 = 0;
      v3 = __builtin_ctz(v3);
  } while (ob13);
  return v3;
}
