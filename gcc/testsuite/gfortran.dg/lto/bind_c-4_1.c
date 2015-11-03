#include <stdlib.h>
#include <stdint.h>
/* interopse with myftype_1 */
typedef struct {
  float val1;
  double val2;
  long double val3;
  float _Complex val4;
  double _Complex val5;
  long double _Complex val6;
  _Bool val7;
  /* FIXME: Fortran define c_char as array of size 1.
     char val8;  */
} myctype_t;


extern void abort(void);
void types_test1(void);
void types_test2(void);
void types_test3(void);
void types_test4(void);
void types_test5(void);
void types_test6(void);
void types_test7(void);
void types_test8(void);
/* declared in the fortran module */
extern myctype_t myVar;

#define test(n)\
  cchr->val##n = 1; types_test##n (); if (cchr->val##n != 2) abort ();

int main(int argc, char **argv)
{
   myctype_t *cchr;
   asm("":"=r"(cchr):"0"(&myVar));
   test(1);
   test(2);
   test(3);
   test(4);
   test(5);
   test(6);
   cchr->val7 = 0; types_test7 (); if (cchr->val7 != 1) abort ();
   /*cchr->val8 = 0; types_test8 (); if (cchr->val8 != 'a') abort ();*/
   return 0;
}

