// GROUPS passed code-generation
// Check that declarations with initializations are executed
// correctly.

extern "C" void printf (char *, ...); 

int main ()
{
	char buff[40] ;
	char *tmp = &buff[0];	// also fails for char *tmp = buff;

	if ((unsigned int) tmp != (unsigned int) &buff[0])
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");

	return 0;
}
