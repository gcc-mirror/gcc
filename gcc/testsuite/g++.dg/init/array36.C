// PR c++/60224

struct A {};

void foo()
{
  bool b[] = (int (A::*)())0;	// { dg-error "" }
}
