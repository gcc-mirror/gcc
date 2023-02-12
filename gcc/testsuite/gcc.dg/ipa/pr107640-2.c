/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-slim"  } */

struct S
{
  int a, b, c;
};

int ellide (int c);
volatile short gi;

void __attribute__((noipa))
consume_s (struct S *p)
{
  gi = p->a;
}

static void __attribute__((noinline))
foo (struct S *p, short *r)
{
  gi = *r;
  if (!__builtin_constant_p (p->b))
    ellide (1);
  consume_s (p);
}

static void __attribute__((noinline))
bar (short *r, struct S *p)
{
  gi = *r;
  if (!__builtin_constant_p (p->c))
    ellide (2);
  consume_s (p);
}

struct S gs;

int main (int argc, char *argv[])
{
  short i = 42;
  gs.a = 10;
  gs.b = 20;
  foo (&gs, &i);
  gs.b = 30;
  gs.c = 40;
  bar (&i, &gs);
  return 0;
}

/* { dg-final { scan-tree-dump-not "ellide" "optimized" { xfail *-*-* } } } */
