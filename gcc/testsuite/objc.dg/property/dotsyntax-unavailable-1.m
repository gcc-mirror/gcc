/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

/* Test the 'dot syntax' with unavailable methods.  */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
}
+ (int) classCount __attribute__ ((unavailable));
+ (void) setClassCount: (int)value __attribute__ ((unavailable));

- (int) count __attribute__ ((unavailable));
- (void) setCount: (int)value __attribute__ ((unavailable));

- (int) classCount2;
- (void) setClassCount2: (int)value;

- (int) count2;
- (void) setCount2: (int)value;
@end

void foo (void)
{
  MyClass *object = nil;


  if (object.count > 0)  /* { dg-error "is unavailable" } */
    object.count = 20;  /* { dg-error "is unavailable" } */

  if (MyClass.classCount < -7)   /* { dg-error "is unavailable" } */
    MyClass.classCount = 11;  /* { dg-error "is unavailable" } */

  if (object.classCount2 > 0)
    object.classCount2 = 19;

  if (object.count2 < -7)
    object.count2 = 74;
}
