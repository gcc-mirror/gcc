interface IFoo
{
	int foo();
}

mixin template MFoo(int N)
{
	int foo() { return N; }
}

class Foo : IFoo
{
	mixin MFoo!(1) t1;
	mixin MFoo!(2) t2;
}
