// Build don't link: 
// GROUPS passed infinite_loop
class A
{
	public:
      A(A &); // ERROR - candidates are
};

class B
{
	public:
	operator A ();
};

class C
{
	public :
	C()
	{
		B	b;
		A a = b;// ERROR - 
	}
};
