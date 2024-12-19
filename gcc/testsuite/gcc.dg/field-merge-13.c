/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ifcombine-details" } */

/* Check that we optimize swapped compares, and that we don't load from objects
   before they're fully initialized.  */

struct pair {
  signed char a;
  signed char b;
} __attribute__ ((aligned (2)));

#define make_pair(a,b) { a, b }

struct s {
  struct pair c;
  struct pair d;
} __attribute__ ((aligned (4)));

struct pair cp = make_pair (127, -1);
struct pair dp = make_pair (42, 0xf1);

struct pair cq = make_pair (127, -1);
struct pair dq = make_pair (42, 0xf1);

struct pair cr = make_pair (-127, -1);
struct pair dr = make_pair (42, 0xff);

static __attribute__ ((noinline, noclone, noipa))
struct pair copy_pair (struct pair c)
{
  return c;
}

static inline
struct s
make_s (struct pair c, struct pair d)
{
  struct s r;
  r.c = copy_pair (c);
  r.d = copy_pair (d);
  return r;
}

void f (void) {
  struct s p = make_s (cp, dp);
  struct s q = make_s (cq, dr);

  if (0
      || p.c.a != q.c.a
      || q.c.b != p.c.b
      || p.d.b != q.d.b
      || q.d.a != p.d.a
      )
    return;
  __builtin_abort ();
}

void g (void) {
  struct s p = make_s (cp, dp);
  struct s q = make_s (cr, dq);

  if (0
      || p.c.a != q.c.a
      || q.c.b != p.c.b
      || p.d.b != q.d.b
      || q.d.a != p.d.a
      )
    return;
  __builtin_abort ();
}

void h (void) {
  struct s p = make_s (cp, dp);
  struct s q = make_s (cq, dq);

  if (0
      || p.c.a != q.c.a
      || q.c.b != p.c.b
      || p.d.b != q.d.b
      || q.d.a != p.d.a
      )
    __builtin_abort ();
  return;
}

int main () {
  f ();
  g ();
  h ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing" 9 "ifcombine" } } */
