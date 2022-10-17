/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-local-pure-const1 -fdump-tree-modref1 -std=gnu++2a"  } */
namespace {
struct B {
	int b;
	struct B *bptr;
	B() {b=1; }
	B(B &src)
	{
		b=src.b;
		bptr=0;
	}
	__attribute__ ((noinline))
	static struct B genB()
	{
		struct B b;
		b.b=2;
		b.bptr = 0;
		return b;
	}
};
}
void linker_error ();
int main()
{
	struct B b1 = B::genB();
	b1.b = 1;
	struct B b2 = B::genB();
	if (b1.b != 1 || b2.bptr == &b2)
		linker_error ();
	return 0;
}
/* { dg-final { scan-tree-dump "Function found to be const: static {anonymous}::B {anonymous}::B::genB" "local-pure-const1"  } } */
/* { dg-final { scan-tree-dump "Retslot flags: not_returned_directly" "modref1" } } */
  
