/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

/* Test the 'dot syntax' with deprecated methods.  */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
} 
+ (int) classCount __attribute__ ((deprecated));
+ (void) setClassCount: (int)value __attribute__ ((deprecated));

- (int) count __attribute__ ((deprecated));
- (void) setCount: (int)value __attribute__ ((deprecated));

- (int) classCount2;
- (void) setClassCount2: (int)value;

- (int) count2;
- (void) setCount2: (int)value;
@end

void foo (void)
{
  MyClass *object = nil;


  if (object.count > 0)  /* { dg-warning "is deprecated" } */
    object.count = 20;  /* { dg-warning "is deprecated" } */

  if (MyClass.classCount < -7)   /* { dg-warning "is deprecated" } */
    MyClass.classCount = 11;  /* { dg-warning "is deprecated" } */

  if (object.classCount2 > 0)
    object.classCount2 = 19;

  if (object.count2 < -7)
    object.count2 = 74;
}
