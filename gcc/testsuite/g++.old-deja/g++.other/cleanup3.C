// Build don't link:
// Special g++ Options: -fno-vtable-thunks
// Origin:  Marc Espie <espie@tetto.liafa.jussieu.fr>

struct A {
	virtual ~A();
	A();
};

struct B: public A {
    B();
};

void f()
{
	static B t[2];
}
