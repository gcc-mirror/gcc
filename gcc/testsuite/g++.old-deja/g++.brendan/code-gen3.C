// { dg-do run  }
// GROUPS passed code-generation
// Check that passing things which are not a multiple of
// 4 bytes in size doesn't mess up other subsequent parameters.

extern "C" int printf (const char *, ...); 

struct base {
	int f1 : 8;
	int f2 : 8;
};

base global_base;

int val1;

int test2 (struct base formal_base, int v1);

int main ()
{
	val1 = 0x5e5e;
	return test2 (global_base, val1);
}

int test2 (struct base formal_base, int v1)
{
	formal_base.f1 = formal_base.f2;	// prevent warnings

	if (v1 != 0x5e5e)
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}
