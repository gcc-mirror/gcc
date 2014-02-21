// PR c++/60219
// { dg-require-effective-target c++11 }

template<typename..., int> void foo();

void bar()
{
  foo<0>;			// { dg-error "" }
}
