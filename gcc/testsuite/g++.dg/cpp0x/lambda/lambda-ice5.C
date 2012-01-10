// PR c++/51227
// { dg-options "-std=c++0x" }

template<int> int foo()
{
  [] (void i) { return 0; } (0); // { dg-error "incomplete|invalid|no match" }
}

void bar()
{
  foo<0>();
}
