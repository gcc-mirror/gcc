/* Check if the '-freplace-objc-classes' option causes the
   __OBJC,__image_info section to be emitted.  This is only
   usable on MacOS X 10.3 and later. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile { target { *-*-darwin* } } } */
/* { dg-options "-freplace-objc-classes" } */

#include "../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>

extern void abort(void);

#define CHECK_IF(expr) if(!(expr)) abort();

@interface Base: Object {
@public
  int a;
  float b;
  char c;
}
- init;
@end

@implementation Base
- init {
  [super init];
  a = 123;
  b = 1.23;
  c = 'c';
  return self;
}
@end

/* { dg-final { scan-assembler "\t.section __OBJC, __image_info.*\n\t.align.*\nL_OBJC_IMAGE_INFO.*:\n\t.long\t0\n\t.long\t1" } } */
