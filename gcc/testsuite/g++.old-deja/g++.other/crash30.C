// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct foo
{
  foo();
  void x();
};

void foo::x() throw(bar)	// { dg-error "" } parse error
{
}

void bar()
{
  foo x;
}
