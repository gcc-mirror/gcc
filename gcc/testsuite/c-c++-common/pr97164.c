/* PR tree-optimization/97164 */
/* { dg-do compile } */

typedef struct { int *a; char b[64]; } A __attribute__((aligned (64)));
struct B { A d[4]; } b;	/* { dg-error "size of array element is not a multiple of its alignment" } */
void foo (void);

int *
bar (void)
{
  struct B *h = &b;
  if (h->d[1].a)
    foo ();
  return h->d[1].a;
}
