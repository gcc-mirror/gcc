/* { dg-additional-options "-ftrivial-auto-var-init=zero" } */

int p;
void g(long);
#define vec16 __attribute__((vector_size(16)))

void l(vec16 long *);
void h()
{
  long inv1;
  vec16 long  inv = {p, inv1};
  g (p);
  l(&inv);
}
