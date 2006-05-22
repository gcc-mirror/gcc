// PR c++/27716
// { dg-do compile }

int foo()
{
  return i ""= i;  // { dg-error "not declared|string constant" }
}
