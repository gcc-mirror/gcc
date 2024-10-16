/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

struct e {
    int (*f)();
    void (*g)();
} * c;
int a;
void *h();
typedef struct { struct e j; } k;
int l() { return a; }
const struct e b = {l};
void m()
{
  k *d = h();
  d->j = b;
  c = (struct e *)d;
  struct e *i = c;
  if (i->f(c))
    while (i->f(c))
      i->g();
}
