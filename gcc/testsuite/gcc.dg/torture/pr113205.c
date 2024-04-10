char a;
char *b, *c;
int d, e, f, g, h;
int *i;

void
foo (void)
{
  unsigned p;
  d = i[0];
  e = i[1];
  f = i[2];
  g = i[3];
  p = d * b[0];
  p += f * c[h];
  p += e * b[h];
  p += g * c[h];
  a = (p + 8000) >> (__SIZEOF_INT__ * __CHAR_BIT__ / 2);
}
