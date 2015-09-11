#include <stdlib.h>
/* interopse with myftype_1 */
typedef struct {
   float *ptr;
   int *ptr2;
} myctype_t;


extern void abort(void);
void types_test(void);
/* declared in the fortran module */
extern myctype_t myVar;

int main(int argc, char **argv)
{
   myctype_t *cptr;
   asm("":"=r"(cptr):"0"(&myVar));
   cptr->ptr = (float *)(size_t) (void *)1;
   cptr->ptr2 = (int *)(size_t) (void *)2;

   types_test();

   if(cptr->ptr != (float *)(size_t) (void *)2)
      abort();
   if(cptr->ptr2 != (int *)(size_t) (void *)2)
      abort();
   myVar.ptr2 = (int *)(size_t) (void *)3;
   types_test();

   if(myVar.ptr != (float *)(size_t) (void *)3)
      abort();
   if(myVar.ptr2 != (int *)(size_t) (void *)3)
      abort();
   return 0;
}

