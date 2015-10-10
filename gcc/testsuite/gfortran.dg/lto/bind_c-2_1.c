#include <stdlib.h>
/* interopse with myftype_1 */
typedef struct {
   unsigned char chr;
   signed char chr2;
} myctype_t;


extern void abort(void);
void types_test(void);
/* declared in the fortran module */
extern myctype_t myVar;

int main(int argc, char **argv)
{
   myctype_t *cchr;
   asm("":"=r"(cchr):"0"(&myVar));
   cchr->chr = 1;
   cchr->chr2 = 2;

   types_test();

   if(cchr->chr != 2)
      abort();
   if(cchr->chr2 != 2)
      abort();
   myVar.chr2 = 3;
   types_test();

   if(myVar.chr != 3)
      abort();
   if(myVar.chr2 != 3)
      abort();
   return 0;
}

