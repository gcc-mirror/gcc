// PR c++/93143
// { dg-do run { target c++11 } }

struct A { char a[2]; };

static constexpr A foo () { return A{1}; }

void bar ()
{
  A a = foo ();
  if (a.a[0] != 1)
    __builtin_abort(); 
}

void foobar ()
{
  A x[] = { foo (), foo () };
  A a = foo ();
  if (a.a[0] != 1)
    __builtin_abort(); 
}

int main()
{
  bar();
  foobar();
}
