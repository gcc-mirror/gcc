// Bug: pointer to pointer is treated as plain pointer.
// PRMS Id: 1767
// Build don't link:

class Foo {
public:
	void method();
};

void func(Foo ** ppFoo) {
	ppFoo->method();	// ERROR - 
}
