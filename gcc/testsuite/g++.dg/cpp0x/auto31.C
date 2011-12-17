// PR c++/51416
// { dg-do compile { target c++11 } }

template<typename T, typename... U> void foo(T, U... u)
{
  auto foo(u...);		// { dg-error "auto" }
}

void bar()
{
  foo(0);
}
