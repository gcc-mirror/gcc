// { dg-do assemble  }
// Bug: A function is not hidden properly by a use of its name in an
// inner scope.

struct A
{
    struct B
    {
	int f;
	B() : f(0) {};
	void g() { f = 0; };
    };
    void f();
    void f(int);
};
