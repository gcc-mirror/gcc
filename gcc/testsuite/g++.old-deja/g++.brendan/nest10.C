// Build don't link: 
// GROUPS passed nested-classes
class A
	{
	public:
		class B
			{
			public:
				int f ();
				void g (int);
			private:
				int b;
			};
	};

int A::B::f ()
	{
	int val=b;
	return val;
	}

void A::B::g (int val)
	{
	b = val;
	}


int main ()
	{
	}
