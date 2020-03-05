// PR c++/91521 - wrong error with operator->.
// { dg-do compile }

struct foo {
	int bar() { return 0; }
	foo* operator->() { return this; }
};

int main()
{
	int pt(foo()->bar());
	return pt;
}
