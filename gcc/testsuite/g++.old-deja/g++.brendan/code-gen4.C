// Special g++ Options: -O
// GROUPS passed code-generation
// Options: -O
//
// Check that when an int value is assigned to a short int, the proper
// half of the int (i.e. the low order half) ends up in the short.
//
// This fails with 1.32.0 with -O and f1() is inline.
//
// Workaround - declare "f1_arg" as type "short int".

extern "C" void printf (char *, ...); 

short int v2;

long v1 = 0x11117777;

inline void f1 (long f1_arg)
{
	v2 = f1_arg;
}

int main ()
{
	f1 (v1);

	if (v2 != 0x00007777)
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");
}
