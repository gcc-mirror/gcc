/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A
{
	~A();
};

/* If we don't create SFT's for the "empty" structure A, bad things
   will happen, and we will fail verification.  */
struct B
{
	int i;
	A a;

	void foo() {}
};

void bar()
{
	B().foo();
}
