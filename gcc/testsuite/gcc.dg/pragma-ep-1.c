/* { dg-do compile { target *-*-osf5* } } */
/* { dg-final { scan-assembler "xyzzy_one" } } */
/* { dg-final { scan-assembler "xyzzy_two" } } */
/* { dg-final { scan-assembler "xyzzz_three" } } */
/* { dg-final { scan-assembler "four" } } */
/* { dg-final { scan-assembler-not "_four" } } */

#ifndef __PRAGMA_EXTERN_PREFIX
#error
#endif

#pragma extern_prefix "xyzzy_"

extern int one(void);
extern int two(void);

#pragma extern_prefix "xyzzz_"

extern int three(void);

#pragma extern_prefix ""

extern int four(void);

int (*p[]) (void) = {
  one, two, three, four
};
