// PR c++/51416
// { dg-options "-std=c++0x" }

template<typename T, typename... U> void foo(T, U... u)
{
  auto foo(u...);		// { dg-error "auto" }
}

void bar()
{
  foo(0);
}
