// PR c++/51227
// { dg-do compile { target c++11 } }

template<int> int foo()
{
  [] (void i) { return 0; } (0); // { dg-error "incomplete|invalid|no match" }
  return 0;
}

void bar()
{
  foo<0>();
}
