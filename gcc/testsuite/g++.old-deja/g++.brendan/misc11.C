// { dg-do assemble  }
// GROUPS passed miscellaneous
// This should not complain about A::f being abstract.
struct	A
{
	virtual int f() = 0;
};

struct	B : virtual A
{
	virtual int f()		{	return 1;	}
	virtual int g() = 0;
};

struct	C: B
{
	int g()			{	return 2;	}
};

C	c;
