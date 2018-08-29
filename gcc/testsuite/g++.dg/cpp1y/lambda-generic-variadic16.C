// PR c++/64095
// { dg-do compile { target c++14 } }

void f()
{
  [](auto...){}();
  [](auto&&...){}();
}
