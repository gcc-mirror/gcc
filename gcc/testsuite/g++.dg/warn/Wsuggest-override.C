// { dg-do compile }
// { dg-options "-std=c++11 -Wsuggest-override" }
struct A
{
	A();
	virtual ~A();
	virtual void f();
	virtual int bar();
	int c();
	operator int();
	virtual operator float();
};

struct B : A
{
	B();
	virtual ~B();
	virtual void f(); // { dg-warning "can be marked override" }
virtual int bar() override;
int c();
operator int();
virtual operator float(); // { dg-warning "can be marked override" }
};
