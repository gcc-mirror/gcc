/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-sra -fno-ipa-cp" } */

typedef struct S
{
  int add_offset;
  int (*call)(int);
} S;

typedef struct Outer
{
  int dummy;
  S inner;
} Outer;

extern int slow (int);
extern void work (int);

static int
target_anc (int x)
{
  return x + 17;
}

static int
target_arith (int x)
{
  return x * 3;
}

static const Outer o1 = {1, {32, target_anc}};

static int
leaf_anc (const S *f, int x)
{
  int r = f->call (x);
  work (r);
  work (r + 1);
  work (r + 2);
  work (r + 3);
  work (r + 4);
  return r;
}

int
other_anc (const S *f, int x)
{
  return leaf_anc (f, x);
}

static int
mid_anc (const Outer *p, int x)
{
  return leaf_anc (&p->inner, x);
}

int
outer_anc (int x)
{
  return mid_anc (&o1, x);
}

static int
leaf_arith (const S *b, int x)
{
  int (*fn)(int) = b->call;
  if (b->add_offset != 42)
    {
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
      x = slow (x);
    }
  return fn (x);
}

int
other_arith (const S *b, int x)
{
  return leaf_arith (b, x);
}

static int
mid_arith (int a, int x)
{
  S q;
  q.add_offset = a + 5;
  q.call = target_arith;
  return leaf_arith (&q, x);
}

int
outer_arith (int x)
{
  return mid_arith (37, x);
}

/* { dg-final { scan-ipa-dump "target_anc\[^\\n\]*inline copy in outer_anc" "inline" } } */
/* { dg-final { scan-ipa-dump "target_arith\[^\\n\]*inline copy in outer_arith" "inline" } } */
