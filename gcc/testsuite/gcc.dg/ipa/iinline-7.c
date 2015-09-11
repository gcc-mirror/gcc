/* Verify that simple indirect calls are inlined even without early
   inlining..  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining -fno-ipa-icf"  } */

extern void abort (void);

struct S
{
  int i;
  void (*f)(struct S *);
  int j,k,l;
};

struct U
{
  struct U *next;
  struct S s;
  short a[8];
};

struct Z
{
  unsigned u;
  void (*f)(struct Z *, int);
  struct Z *next;
};

static struct Z *gz;
static struct U *gu;
static int gr = 111;
char gc[1024];

static __attribute__ ((noinline, noclone)) struct U *
get_u (void)
{
  return (struct U *) &gc;
}

static void wrong_target_1 (struct S *s)
{
  abort ();
}

static void wrong_target_2 (struct S *s)
{
  abort ();
}

static void wrong_target_3 (struct S *s)
{
  abort ();
}

static void wrong_target_4 (struct S *s)
{
  abort ();
}

static void good_target (struct Z *z, int i)
{
  gr = 0;
}

static void good_target_4 (struct S *s)
{
  gr = 0;
}

static void g1 (struct S *s)
{
  struct Z *z = (struct Z*) s;
  z->f (z, 8);
}

static void f1 (struct U *u)
{
  gz->f = good_target;
  g1 (&u->s);
}

static void g2 (struct Z *z)
{
  z->f (z, 8);
}

static void f2 (struct U *u)
{
  gz->f = good_target;
  g2 ((struct Z*) &u->s);
}

static void h3 (struct Z *z)
{
  z->f (z, 8);
}

static void g3 (struct S *s)
{
  h3 ((struct Z*) s);
}

static void f3 (struct U *u)
{
  gz->f = good_target;
  g3 (&u->s);
}

static void h4 (struct S *s)
{
  s->f (s);
}

static void g4 (struct U *u)
{
  h4 (&u->s);
}

static inline __attribute__ ((flatten)) void f4 (struct Z *z)
{
  gu->s.f = good_target_4;
  g4 ((struct U *) z);
}

int main (int argc, char **argv)
{
  struct U *u = get_u ();
  u->next = u;
  u->s.i = 5678;
  u->s.f = wrong_target_1;
  u->s.j = 1234;
  gz = (struct Z *) &u->s;
  f1 (u);

  u = get_u();
  u->s.i = 9999;
  u->s.f = wrong_target_2;
  gz = (struct Z *) &u->s;
  f2 (u);

  u = get_u();
  u->s.i = 9998;
  u->s.f = wrong_target_3;
  gz = (struct Z *) &u->s;
  f3 (u);

  u = get_u();
  u->s.i = 9998;
  u->s.f = wrong_target_4;
  gu = u;
  f4 ((struct Z *) u);
  return gr;
}


/* { dg-final { scan-ipa-dump-not "wrong_target\[^\\n\]*inline copy in" "inline"  } } */
