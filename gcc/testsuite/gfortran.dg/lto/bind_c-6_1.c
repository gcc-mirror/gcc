#include <stdlib.h>
/* declared in the fortran module */
extern size_t myVar, myVar2;
void types_test(void);


extern void abort(void);

int main(int argc, char **argv)
{
   size_t *myptr, *myptr2;
   asm("":"=r"(myptr):"0"(&myVar));
   asm("":"=r"(myptr2):"0"(&myVar2));
   *myptr = 1;
   *myptr2 = 2;
   types_test();
   if (*myptr != 2)
	abort ();
   if (*myptr2 != 2)
	abort ();
   *myptr2 = 3;
   types_test();
   if (*myptr != 3)
	abort ();
   if (*myptr2 != 3)
	abort ();
   return 0;
}

