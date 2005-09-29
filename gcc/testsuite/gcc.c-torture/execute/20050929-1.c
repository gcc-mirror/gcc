/* PR middle-end/24109 */

extern void abort (void);

struct A { int i; int j; };
struct B { struct A *a; struct A *b; };
struct C { struct B *c; struct A *d; };
struct C e = { &(struct B) { &(struct A) { 1, 2 }, &(struct A) { 3, 4 } }, &(struct A) { 5, 6 } };

int
main (void)
{
  if (e.c->a->i != 1 || e.c->a->j != 2)
    abort ();
  if (e.c->b->i != 3 || e.c->b->j != 4)
    abort ();
  if (e.d->i != 5 || e.d->j != 6)
    abort ();
  return 0;
}
