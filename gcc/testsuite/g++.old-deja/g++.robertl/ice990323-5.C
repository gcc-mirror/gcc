// { dg-do assemble  }
// check attempting to throw an overloaded function

struct A {
	void f(int);
	void f(long);
};

void g()
{
	throw &A::f; // { dg-error "" } insufficient context
}
