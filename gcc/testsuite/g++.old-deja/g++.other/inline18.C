// { dg-do assemble  }
// { dg-options "-O3" }
// Origin: Jakub Jelinek <jakub@redhat.com>

static void foo (int a)
{
  a = a;
}

static void bar (void)
{
  foo (-1);
}
