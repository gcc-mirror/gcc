/* { dg-do compile { target lp64 } } */

int a;
long b[1][9];
typedef long V __attribute__((vector_size (16), may_alias));

void
foo ()
{
  V *c = (V *) ((char *) b + -9060696663385964544);
  *c = (V) { 1, 1 };
  c++;
  *c = (V) { 1, 1 };
  c++;
  *c = (V) { 1, 1 };
  c++;
  *c = (V) { 1, 1 };
  long __attribute__((may_alias)) *d = (long *) ((char *) b + 162675373468811328);
  *d = 1;
}
