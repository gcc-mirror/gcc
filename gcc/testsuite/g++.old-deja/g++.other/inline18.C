// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>
// Special g++ Options: -O3

static void foo (int a)
{
  a = a;
}

static void bar (void)
{
  foo (-1);
}
