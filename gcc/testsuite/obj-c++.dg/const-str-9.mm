/* Test if ObjC constant strings get placed in the correct section.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"

@interface NSConstantString: Object {
  char *cString;
  unsigned int len;
}
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _NSConstantStringClassReference;
#else
extern struct objc_class _NSConstantStringClassReference;
#endif

const NSConstantString *appKey = @"MyApp";

/* { dg-final { scan-assembler ".section __OBJC, __cstring_object" } } */
/* { dg-final { scan-assembler ".long\t__NSConstantStringClassReference\n\t.long\t.*\n\t.long\t5\n\t.data"  { target { *-*-darwin* && { ! lp64 } } }} } */
/* { dg-final { scan-assembler ".quad\t__NSConstantStringClassReference\n\t.quad\t.*\n\t.long\t5\n\t.space 4\n\t.data" { target { *-*-darwin* && { lp64 } } } } } */
