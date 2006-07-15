// PR c++/28249
// { dg-do compile }

void foo()
{
  try {}
  catch (long long long) {}  // { dg-error "long long long" }
}
