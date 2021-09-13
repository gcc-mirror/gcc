/* Verify that fix-it printing doesn't ICE when there are multiple
   fix-it hints on a very long line after LINE_MAP_MAX_COLUMN_NUMBER.  */

/* { dg-options "-Wall -no-integrated-cpp -fdiagnostics-show-caret" } */
/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */

typedef struct {
} REFERENCE;
#define LIM2() LIM1()
#define LIM3() LIM2() LIM2() LIM2() LIM2() LIM2() LIM2()
#define LIM4()                                                                 \
  LIM3() LIM3() LIM3() LIM3() LIM3() LIM3() LIM3() LIM3() LIM3() LIM3()
#define LIM5()                                                                 \
  LIM4() LIM4() LIM4() LIM4() LIM4() LIM4() LIM4() LIM4() LIM4() LIM4()
#define LIM1() DEF(),
REFERENCE references[] = {LIM5()};
