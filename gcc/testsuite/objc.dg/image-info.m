/* Check if the '-freplace-objc-classes' option causes the
   __OBJC,__image_info section to be emitted.  This is only
   usable on MacOS X 10.3 and later. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile { target { *-*-darwin* } } } */
/* { dg-skip-if "NeXT-only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-freplace-objc-classes" } */

#include "../objc-obj-c++-shared/F-NSObject.h"

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort();

@interface NSObject (TEST_SUITE_C1)
- init;
@end
@implementation NSObject (TEST_SUITE_C1)
- init {return self;}
@end

@interface Base: NSObject {
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

/* { dg-final { scan-assembler "\t.section __OBJC, __image_info.*\n\t.align.*\nL_OBJC_ImageInfo.*:\n\t.long\t0\n\t.long\t1"  { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler "\t.section __DATA, __objc_imageinfo.*\n\t.align.*\nL_OBJC_ImageInfo.*:\n\t.long\t0\n\t.long\t17" { target { *-*-darwin* && { lp64 } } } } } */
