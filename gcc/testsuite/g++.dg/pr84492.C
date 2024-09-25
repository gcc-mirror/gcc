// { dg-do compile }
// { dg-options "-fpermissive" }

template<int> int foo()
{
  return ({ foo; }); // { dg-warning "insufficient context" }
}

int bar()
{
  return ({ foo; }); // { dg-error "insufficient context" }
}

void bar(int);

typedef void (*bart)(int);

bart barf()
{
  return ({ bar; }); // { dg-error "insufficient context" }
}

bool bark()
{
  return ({ barf; }); // ok, no overload
}

template <typename T>
class C
{
  static int f();
  bool g()
  {
    return ({ f; }); // ok, no overload
  }
  bool g(int)
  {
    return ({ g; }); // { dg-warning "insufficient context" }
  }
};
