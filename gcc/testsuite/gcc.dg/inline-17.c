/* Test __attribute__((gnu_inline)).  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler-not "func2" } } */
/* { dg-final { scan-assembler "func3" } } */
/* { dg-final { scan-assembler "func4" } } */

#if __STDC_VERSION__ >= 199901L
# define inline __attribute__((gnu_inline)) inline
#endif

extern inline int func1 (void) { return 0; }
inline int func1 (void) { return 1; }

extern int func2 (void);
extern inline int func2 (void) { return 2; }

inline int func3 (void);
inline int func3 (void) { return 3; }

extern int func4 (void);
extern inline int func4 (void) { return 4; }
int func4 (void) { return 5; }
