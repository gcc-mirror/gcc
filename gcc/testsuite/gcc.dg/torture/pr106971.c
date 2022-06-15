/* { dg-do compile } */

void a()
{
  int b;
  int c;
  int d = (__INTPTR_TYPE__)a;
  _Complex float *e = (_Complex float *)a;
  for (;;) {
    (*e += d) / b ?: 0;
  }
}
