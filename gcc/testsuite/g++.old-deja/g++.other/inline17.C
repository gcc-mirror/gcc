// { dg-do assemble  }
// { dg-options "-O3" }
// Origin: Jakub Jelinek <jakub@redhat.com>

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
