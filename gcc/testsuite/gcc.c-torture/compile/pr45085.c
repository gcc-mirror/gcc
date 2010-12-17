/* { dg-options "-O2  -Wuninitialized" } */
struct S { char *s1; long s2; };
struct T { int t1; long t2; long t3; };
extern int fn2 (void);
extern int fn3 (struct T);
extern struct T fn4 ();
extern int fn5 (char **, long *, int);
extern void fn6 (void);
extern void fn7 (void *);
struct S *fn10 ();
static int p;
static void *q;
extern struct T r;

static struct T
fn8 (struct T x, int y)
{
  struct S *u = fn10 ();
  int v = fn5 (&u->s1, &u->s2, 0);
  while (1)
    {
      if (p)
fn6 ();
      if (fn3 (x))
return fn4 ();
      if (y & 1)
return r;
      v = fn5 (&u->s1, &u->s2, 1);
    }
}

struct T
fn9 (struct T x, int y)
{
  struct T t = fn8 (x, y);
  if (fn2 ())
    fn7 (q);
  return t;
}

void *
fn1 (void)
{
  return fn9;
}
