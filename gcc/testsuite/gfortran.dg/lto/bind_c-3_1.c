#include <stdlib.h>
#include <stdint.h>
/* interopse with myftype_1 */
typedef struct {
  int val1;
  short int val2;
  long int val3;
  long long int val4;
  size_t val5;
  int8_t val6;
  int16_t val7;
  int32_t val8;
  int64_t val9;
  int_least8_t val10;
  int_least16_t val11;
  int_least32_t val12;
  int_least64_t val13;
  int_fast8_t val14;
  int_fast16_t val15;
  int_fast32_t val16;
  int_fast64_t val17;
  intmax_t val18;
  intptr_t val19;
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
void types_test9(void);
void types_test10(void);
void types_test11(void);
void types_test12(void);
void types_test13(void);
void types_test14(void);
void types_test15(void);
void types_test16(void);
void types_test17(void);
void types_test18(void);
void types_test19(void);
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
   test(7);
   test(8);
   test(9);
   test(10);
   test(11);
   test(12);
   test(13);
   test(14);
   test(15);
   test(16);
   test(17);
   test(18);
   test(19);
   return 0;
}

