// { dg-do assemble  }
// Bug: pointer to pointer is treated as plain pointer.
// PRMS Id: 1767

class Foo {
public:
	void method();
};

void func(Foo ** ppFoo) {
	ppFoo->method();	// { dg-error "" } 
}
