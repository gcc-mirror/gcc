// PR c++/44311
// { dg-options -std=c++0x }

enum class A { Val0, Val1 };

void foo (A a, int i)
{
  switch (a)
    {
    case A::Val0: break;
    case 1: break;		// { dg-error "" }
    }

  switch (i)
    {
    case A::Val0: break;	// { dg-error "" }
    case 1: break;
    case 2.0: break;            // { dg-error "" }
    }
}
