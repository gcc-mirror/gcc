// { dg-do run  }
// GROUPS passed code-generation
// Check that declarations with initializations are executed
// correctly.

extern "C" int printf (const char *, ...); 

int main ()
{
	char buff[40] ;
	char *tmp = &buff[0];	// also fails for char *tmp = buff;

	if ((__SIZE_TYPE__) tmp != (__SIZE_TYPE__) &buff[0])
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}
