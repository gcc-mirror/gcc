// { dg-do assemble  }
namespace B{
 void f();
}

using namespace B;

void g()
{
	::f();
}
