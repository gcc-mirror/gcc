/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-cp-details -fdump-tree-optimized-slim"  } */
/* { dg-add-options bind_pic_locally } */

struct S
{
  int a, b, c;
};

void *blah(int, void *);

static void __attribute__ ((noinline))
foo (struct S *p)
{
  int i, c = p->c;
  int b = p->b;
  void *v = (void *) p;

  for (i= 0; i< c; i++)
    v = blah(b + i, v);
}

void
entry (void)
{
  struct S s;
  s.a = 1;
  s.b = 64;
  s.c = 32;
  foo (&s);
}

/* { dg-final { scan-ipa-dump "Creating a specialized node of foo.*for all known contexts" "cp" } } */
/* { dg-final { scan-ipa-dump-times "Aggregate replacements:" 2 "cp" } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
/* { dg-final { scan-tree-dump-not "->c;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
