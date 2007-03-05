/* Test -fgnu89-extern-inline.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -fgnu89-inline" } */
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler-not "func2" } } */
/* { dg-final { scan-assembler "func3" } } */
/* { dg-final { scan-assembler "func4" } } */

#ifndef __GNUC_GNU_INLINE__
#error __GNUC_GNU_INLINE__ is not defined
#endif

#ifdef __GNUC_STDC_INLINE__
#error __GNUC_STDC_INLINE__ is defined
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
