#include <stdlib.h>
/* declared in the fortran module */
extern int (*myVar) (int);
extern float (*myVar2) (float);
void types_test(void);


extern void abort(void);

int main(int argc, char **argv)
{
   int (**myptr) (int);
   float (**myptr2) (float);
   asm("":"=r"(myptr):"0"(&myVar));
   asm("":"=r"(myptr2):"0"(&myVar2));
   *myptr = (int (*) (int)) (size_t) (void *)1;
   *myptr2 = (float (*) (float)) (size_t) (void *)2;
   types_test();
   if (*myptr != (int (*) (int)) (size_t) (void *)2)
	abort ();
   if (*myptr2 != (float (*) (float)) (size_t) (void *)2)
	abort ();
   *myptr2 = (float (*) (float)) (size_t) (void *)3;
   types_test();
   if (*myptr != (int (*) (int)) (size_t) (void *)3)
	abort ();
   if (*myptr2 != (float (*) (float)) (size_t) (void *)3)
	abort ();
   return 0;
}

