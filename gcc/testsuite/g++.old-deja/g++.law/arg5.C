// Build don't link: 
// GROUPS passed arg-matching
extern double pow(double,int*);

extern "C" {
  extern int    printf(char*,...);
  extern double pow(double, double);
}

int main()
{
	if (pow (2.0, 3.0) != 8.0)
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");
}
