// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>
// Special g++ Options: -O3

struct foo
{
  char a;
  foo ();
  void bar ();
  void baz (char c);
};

void foo::baz (char c)
{
  if (c != a)
    a = c;
}

void foo::bar ()
{
  baz (1);
}
