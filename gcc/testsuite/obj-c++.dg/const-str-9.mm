/* Test if ObjC constant strings get placed in the correct section.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-options "-fnext-runtime" } */
/* { dg-do compile { target *-*-darwin* } } */

#include <objc/Object.h>

@interface NSConstantString: Object {
  char *cString;
  unsigned int len;
}
@end

extern struct objc_class _NSConstantStringClassReference;

const NSConstantString *appKey = @"MyApp";

/* { dg-final { scan-assembler ".section __OBJC, __cstring_object" } } */
/* { dg-final { scan-assembler ".long\t__NSConstantStringClassReference\n\t.long\t.*\n\t.long\t5\n\t.data" } } */
