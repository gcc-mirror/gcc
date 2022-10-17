/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse2-details" } */
/* { dg-final { scan-tree-dump-not "Deleted dead store" "dse2" } } */

struct foo { unsigned long bar; };

unsigned y;

static int __attribute__ ((__noinline__, __noclone__))
wrapped (struct foo *p, int i);

static int wrapper (struct foo *p);

static int __attribute__ ((__noclone__))
wrapper (struct foo *p) {
  return wrapped (p, 1);
}

static int __attribute__ ((__noinline__, __noclone__))
dind (struct foo **pp);

int __attribute__ ((__noclone__, __no_reorder__))
xfn () {
  struct foo x = { 0xBADC0FFE };
  struct foo *p = &x;
  return dind (&p);
}

static int __attribute__ ((__noinline__, __no_reorder__))
wrapped (struct foo *p, int i) {
  return p->bar + i == y++;
}

static int __attribute__ ((__noinline__, __noclone__, __no_reorder__))
dind (struct foo **pp) {
  wrapper (*pp);
  return 0;
}
