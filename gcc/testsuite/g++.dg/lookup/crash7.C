// PR c++/35323
// { dg-options "" }

void foo(int);

void bar()
{
  foo(1r); // { dg-error "unnamed-fixed" }
}
