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

static void __attribute__ ((noinline))
bar (struct S *p)
{
  foo (p);
}

static void __attribute__ ((noinline))
bar_2 (struct S *p)
{
  foo (p);
}

void
entry1 (int c)
{
  struct S s;
  int i;

  for (i = 0; i<c; i++)
    {
      s.a = 1;
      s.b = 64;
      s.c = 32;
      bar (&s);
    }
  s.c = 2;
  bar (&s);
}

void
entry2 (int c)
{
  struct S s;
  int i;

  for (i = 0; i<c; i++)
    {
      s.a = 6;
      s.b = 64;
      s.c = 32;
      bar_2 (&s);
    }
  s.c = 2;
  foo (&s);
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of foo/\[0-9\]*\\." 2 "cp" } } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of bar/\[0-9\]*\\." 2 "cp" } } */
/* { dg-final { scan-ipa-dump "Creating a specialized node of bar_2.*for all known contexts" "cp" } } */
/* { dg-final { scan-ipa-dump-times "Aggregate replacements:" 10 "cp" } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
/* { dg-final { scan-tree-dump-not "->c;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
