// Build don't link: 
namespace B{
 void f();
}

using namespace B;

void g()
{
	::f();
}
