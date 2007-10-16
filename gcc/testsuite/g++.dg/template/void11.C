// PR c++/31446

template<void> struct A // { dg-error "valid type" }

{
  template<int> friend void foo();
};

void bar()
{
  foo<0>(); // { dg-error "not declared|primary-expression" }
}
