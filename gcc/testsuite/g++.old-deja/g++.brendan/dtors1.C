// GROUPS passed destructors
// Check that when an object of a derived class is (implicitly)
// destructed (on exit from the block in which it is declared)
// that the destructor for the base class also gets executed.
//
// (also check that this execution doesn't seg-fault)

extern "C" void printf (char *, ...); 

int derived_destructed;
int base_destructed;

struct base {
  int base_data_member;

  base()
  {
    base_data_member = 0x5e5e;
  }
  ~base()
  {
    base_destructed = 0x781f;
  }
};

struct derived : public base {
  int derived_data_member;

  derived()
  {
    derived_data_member = 0xe5e5;
  }
  ~derived()
  {
    derived_destructed = 0xf178;
  }
};


void test2 ();

int main ()
{
	test2 ();
	if ((base_destructed != 0x781f) || (derived_destructed != 0xf178))
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");
}

void test2 ()
{
	derived derived_object;

	derived_object.derived_data_member = 99;
}
