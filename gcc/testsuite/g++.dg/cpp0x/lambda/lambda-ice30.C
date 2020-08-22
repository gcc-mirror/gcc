// PR c++/84518
// { dg-do compile { target c++11 } }

template<int> void foo()
{
  int x[=];  // { dg-error "" }
  [&x]{};
}

void bar()
{
  foo<0>();			// { dg-prune-output "not declared" }
}
