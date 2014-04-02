// PR c++/38795
// { dg-do compile { target c++11 } }

template<typename... T> int foo(int i)
{
  return *reinterpret_cast<T*>(i);	// { dg-error "not expanded with|T" }
}

void bar(int i)
{
  foo<int>(i);
}
