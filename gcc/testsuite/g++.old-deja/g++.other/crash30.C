// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct foo
{
  foo();
  void x();
};

void foo::x() throw(bar)	// { dg-error "" } parse error
{				// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } .-1 }
}

void bar()
{
  foo x;
}
