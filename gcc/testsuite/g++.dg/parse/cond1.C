// PR c++/18389

void foo()
{
  for (; struct A {}; ); // { dg-error "" }
}
