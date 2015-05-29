/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining"  } */

extern void abort (void);

struct S
{
  int i;
  void (*f)(struct S *);
  int j,k,l;
};

struct Z
{
  unsigned u;
  void (*f)(struct Z *, int);
  struct Z *next;
};

static struct S *gs;
static int gr = 111;
char gc[1024];

static __attribute__ ((noinline, noclone)) struct S *
get_s (void)
{
  return (struct S *) &gc;
}

static void wrong_target (struct S *s)
{
  abort ();
}

static void good_target (struct S *s)
{
  gr = 0;
}

static void g1 (struct S *s)
{
  s->f (s);
}

static void f2 (struct Z *z)
{
  gs->f = good_target;
  g1 ((struct S *) z);
}

static inline __attribute__ ((flatten)) void f1 (struct S *s)
{
  f2 ((struct Z *) s);
}

int main (int argc, char **argv)
{
  struct S *s = get_s();
  s->i = 5678;
  s->f = wrong_target;
  s->j = 1234;
  gs = s;
  f1 (s);

  return gr;
}


/* { dg-final { scan-ipa-dump-not "wrong_target\[^\\n\]*inline copy in" "inline"  } } */
