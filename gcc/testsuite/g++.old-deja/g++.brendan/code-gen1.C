// { dg-do run  }
// GROUPS passed code-generation
// Check that sub-word sized structs/classes are passed correctly
// if the struct/class has a constructor (i.e. ANY constructor).

extern "C" int printf (const char *, ...); 

struct base {
	unsigned int f1 : 8;
	unsigned int f2 : 8;

	base (int ii)
	{
	}
};

base global_base (7);

int test2 (base formal_base);

int main ()
{
	global_base.f1 = 0x55;
	global_base.f2 = 0xee;

	if (test2 (global_base) == 0)
	  printf ("PASS\n");
	else
	  { printf ("FAIL\n"); return 1; }

	return 0;
}

int test2 (base formal_base)
{
	if (formal_base.f1 != global_base.f1)
		return -1;
	if (formal_base.f2 != global_base.f2)
		return -1;
	return 0;
}
