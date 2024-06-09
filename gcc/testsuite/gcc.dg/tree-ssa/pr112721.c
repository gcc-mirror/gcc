/* { dg-do run } */
/* { dg-options "-O1" } */

unsigned * volatile gv;

struct a {
  int b;
};
int c, e;
long d;
unsigned * __attribute__((noinline))
f(unsigned *g) {
  for (; c;)
    e = d;
  return gv ? gv : g;
}
int main() {
  int *h;
  struct a i = {8};
  int *j = &i.b;
  h = (unsigned *) f(j);
  *h = 0;
  if (i.b != 0)
    __builtin_abort ();
  return 0;
}
