// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*

struct foo
{
  foo();
  void x();
};

void foo::x() throw(bar)	// ERROR - parse error
{
}

void bar()
{
  foo x;
}
