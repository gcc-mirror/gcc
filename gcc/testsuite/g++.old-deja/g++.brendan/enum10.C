// Build don't link: 
// GROUPS passed enums
class Type
{
	public:

		enum name
		{
			A
		};
};

class A
{
};

class B: public A
{
	public:
		B();
};
