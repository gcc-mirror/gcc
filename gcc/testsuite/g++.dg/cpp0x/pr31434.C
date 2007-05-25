// { dg-options "-std=gnu++0x" }
template<typename... T> int foo(const T&) // { dg-error "not expanded with|T" }
{
 union { T t; }; // { dg-error "not expanded with|T" }
 return t;
}

void bar()
{
  foo(0); // { dg-error "no matching" }
}
