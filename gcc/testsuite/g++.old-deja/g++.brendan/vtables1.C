// { dg-do run  }
// GROUPS passed virtual-tables
// Check that virtual function tables are generated and used
// even for struct's/classes which have no explicit constructors.

extern "C" int printf (const char *, ...); 

int base_func_member_1_called = 0;
int derived_func_member_1_called = 0;

struct base {
	virtual void func_member_1 ()
	{
		base_func_member_1_called++;
	}
};

struct derived : public base {
	virtual void func_member_1 ()
	{
		derived_func_member_1_called++;
	}

	virtual void func_member_2 ()
	{
	}
};

base* bp1;
base* bp2;

int main ()
{
	bp1 = new base;
	bp2 = new derived;

	bp1->func_member_1 ();
	bp2->func_member_1 ();

	printf ("PASS\n");

	return 0;
}
