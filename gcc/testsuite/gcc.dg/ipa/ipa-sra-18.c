/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra"  } */

struct S
{
  long a, b;
};

extern void leaf_a (int );
extern void leaf_b (int, int);
extern void leaf_c (int, int);

extern void leaf_sa (struct S);

static void baz (int i, int j, int k, int l, struct S a, struct S b);

extern int gi;

static void  __attribute__((noinline))
foo (int i, int j, int k, int l, struct S a, struct S b)
{
  gi += l;
  baz (i, j, k, l, a, b);
}

static void __attribute__((noinline))
bar (int i, int j, int k, int l, struct S a, struct S b)
{
  foo (i, j, k, l, a, b);
  leaf_sa (b);
}


static void __attribute__((noinline))
baz (int i, int j, int k, int l, struct S a, struct S b)
{
  if (--k)
    bar (i, j, k, l, a, b);
  leaf_b (i, k);
}

void
entry (int i, int j, int k, int l, struct S a, struct S b)
{
  foo (i, j, k, l, a, b);
}

/* { dg-final { scan-ipa-dump-times "Will remove parameter 1" 3 "sra" } } */
/* { dg-final { scan-ipa-dump-times "Will remove parameter 4" 3 "sra" } } */
