// { dg-do assemble  }
// GROUPS passed infinite_loop
class A
{
	public:
      A(A &); // { dg-message "note" "" { target c++14_down } }
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
		A a = b;// { dg-error "rvalue" "" { target c++14_down } }
	}
};
