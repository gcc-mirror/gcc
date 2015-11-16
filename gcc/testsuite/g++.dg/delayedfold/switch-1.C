// PR c++/68362
// { dg-do compile { target c++11 } }

enum class A { foo };
enum E { bar };

void
fn1 (const A a)
{
  switch (a)
  case A::foo:;
}

void
fn2 (E e)
{
  switch (e)
  case bar:;
}
