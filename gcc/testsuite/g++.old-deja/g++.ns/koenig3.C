//Check association of member pointer in overload resolution.
struct A {
	int m_val;
	friend int operator ->* (A & other, int A::*pm)
		{ return 31; }
};

int A::*pi = & A::m_val;

int
main(void)
{
	A c;
	c.m_val = 42;
	int j = c ->* pi;

	if (j == 31)
		return 0;
	else
		return 1;
}	
