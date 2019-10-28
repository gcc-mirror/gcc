/* { dg-do compile } */
/* { dg-additional-options "-mcpu=power8" { target powerpc*-*-* } } */
/* { dg-additional-options "-w" { target avr-*-* } } */

long f;
void a();
void *g()
{
  char h[] = {}, j[] = {}, k[] = {}, l[] = {}, m[] = {}, n[] = {}, o[] = {},
       q[] = {}, r[] = {};
  static const char i[] = {6, 0};
  const char *nops[] = {h, i, j, k, l, m, n, o, q, r};
  long s = 2;
  void *fill = a;
  char *p = fill;
  while (f) {
      void *b = p;
      const void *c = nops[1];
      long d = s, e = __builtin_object_size(b, 0);
      __builtin___memcpy_chk(b, c, d, e);
      p += s;
      f -= s;
  }
  return fill;
}
