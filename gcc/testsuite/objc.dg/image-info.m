/* Check if the '-freplace-objc-classes' option causes the
   __OBJC,__image_info section to be emitted.  This is only
   usable on MacOS X 10.3 and later. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-freplace-objc-classes" } */
/* { dg-do compile { target *-*-darwin* } } */

#ifndef __NEXT_RUNTIME__
#error Feature not currently supported by the GNU runtime
#endif

#include <objc/objc.h>
#include <objc/Object.h>

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

/* { dg-final { scan-assembler "\n.data\n.section __OBJC, __image_info\n\t.align.*\nL_OBJC_IMAGE_INFO.*:\n\t.long\t0\n\t.long\t1\n.data\n.objc_module_info\n" } } */
