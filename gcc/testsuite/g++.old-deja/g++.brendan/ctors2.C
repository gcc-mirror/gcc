// GROUPS passed constructors
// Check that sub-words sized class members are correctly set
// by constructors.

extern "C" void printf (char *, ...); 

struct base {
	int f1 : 8;
	int f2 : 8;
	base (int arg1, int arg2);
};


base global_base(0x55, 0x7e);

int main ()
{
	if ((global_base.f1 != 0x55) || (global_base.f2 != 0x7e))
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");
}

base::base(int arg1, int arg2)
{
	f1 = arg1;
	f2 = arg2;
};
