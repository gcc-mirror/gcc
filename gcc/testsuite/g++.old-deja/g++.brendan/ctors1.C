// { dg-do run  }
// GROUPS passed constructors
// Check that global level object constructors get called.

extern "C" int printf (const char *, ...); 

struct base {
	int f1;
	int f2;
	base (int arg1, int arg2);
};


base global_base(0x55, 0xff);

int main ()
{
	if ((global_base.f1 != 0x55) || (global_base.f2 != 0xff))
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");
}

base::base(int arg1, int arg2)
{
	f1 = arg1;
	f2 = arg2;
}
