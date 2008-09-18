// { dg-do assemble  }
// GROUPS passed infinite_loop
class A
{
	public:
      A(A &); // { dg-message "candidates" }
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
		A a = b;// { dg-error "match" } 
	}
};
