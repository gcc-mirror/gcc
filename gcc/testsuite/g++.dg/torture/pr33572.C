// { dg-do run }
// { dg-options "-std=c++98" }

#include <vector>
#include <memory>

struct Foo { virtual void f() {} };

int main(int argc, char**)
{
	std::auto_ptr<Foo> foo;
	if (argc >= 0) {
		foo.reset(new Foo());
	} else {
		std::vector<int> v;
	}
	Foo* p = foo.release();
	p->f();
}
