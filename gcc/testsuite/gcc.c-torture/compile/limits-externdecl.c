/* { dg-skip-if "ptxas runs out of memory" { nvptx-*-* } } */
/* { dg-skip-if "Array too big" { "pdp11-*-*" } { "-mint32" } } */

/* { dg-require-effective-target int32plus } */
/* Inspired by the test case for PR middle-end/52640.  */

typedef struct
{
    char *value;
} REFERENCE;

/* Add a few "extern int Xxxxxx ();" declarations.  */
#undef DEF
#undef LIM1
#undef LIM2
#undef LIM3
#undef LIM4
#undef LIM5
#undef LIM6
#define DEF(x) 	extern int x ()
#define LIM1(x) DEF(x##0); DEF(x##1); DEF(x##2); DEF(x##3); DEF(x##4); \
		DEF(x##5); DEF(x##6); DEF(x##7); DEF(x##8); DEF(x##9);
#define LIM2(x) LIM1(x##0) LIM1(x##1) LIM1(x##2) LIM1(x##3) LIM1(x##4) \
		LIM1(x##5) LIM1(x##6) LIM1(x##7) LIM1(x##8) LIM1(x##9)
#define LIM3(x) LIM2(x##0) LIM2(x##1) LIM2(x##2) LIM2(x##3) LIM2(x##4) \
		LIM2(x##5) LIM2(x##6) LIM2(x##7) LIM2(x##8) LIM2(x##9)
#define LIM4(x) LIM3(x##0) LIM3(x##1) LIM3(x##2) LIM3(x##3) LIM3(x##4) \
		LIM3(x##5) LIM3(x##6) LIM3(x##7) LIM3(x##8) LIM3(x##9)
#define LIM5(x) LIM4(x##0) LIM4(x##1) LIM4(x##2) LIM4(x##3) LIM4(x##4) \
		LIM4(x##5) LIM4(x##6) LIM4(x##7) LIM4(x##8) LIM4(x##9)
#define LIM6(x) LIM5(x##0) LIM5(x##1) LIM5(x##2) LIM5(x##3) LIM5(x##4) \
		LIM5(x##5) LIM5(x##6) LIM5(x##7) LIM5(x##8) LIM5(x##9)
LIM5 (X);

/* Add references to them, or GCC will simply ignore the extern decls.  */
#undef DEF
#undef LIM1
#undef LIM2
#undef LIM3
#undef LIM4
#undef LIM5
#undef LIM6
#define DEF(x)	(char *) x
#define LIM1(x) DEF(x##0), DEF(x##1), DEF(x##2), DEF(x##3), DEF(x##4), \
		DEF(x##5), DEF(x##6), DEF(x##7), DEF(x##8), DEF(x##9),
#define LIM2(x) LIM1(x##0) LIM1(x##1) LIM1(x##2) LIM1(x##3) LIM1(x##4) \
		LIM1(x##5) LIM1(x##6) LIM1(x##7) LIM1(x##8) LIM1(x##9)
#define LIM3(x) LIM2(x##0) LIM2(x##1) LIM2(x##2) LIM2(x##3) LIM2(x##4) \
		LIM2(x##5) LIM2(x##6) LIM2(x##7) LIM2(x##8) LIM2(x##9)
#define LIM4(x) LIM3(x##0) LIM3(x##1) LIM3(x##2) LIM3(x##3) LIM3(x##4) \
		LIM3(x##5) LIM3(x##6) LIM3(x##7) LIM3(x##8) LIM3(x##9)
#define LIM5(x) LIM4(x##0) LIM4(x##1) LIM4(x##2) LIM4(x##3) LIM4(x##4) \
		LIM4(x##5) LIM4(x##6) LIM4(x##7) LIM4(x##8) LIM4(x##9)
#define LIM6(x) LIM5(x##0) LIM5(x##1) LIM5(x##2) LIM5(x##3) LIM5(x##4) \
		LIM5(x##5) LIM5(x##6) LIM5(x##7) LIM5(x##8) LIM5(x##9)
REFERENCE references[] = {
  LIM5 (X)
  0
};
