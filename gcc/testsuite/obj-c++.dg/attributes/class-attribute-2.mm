/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>

__attribute__ ((deprecated)) 
@interface DeprecatedClass
{
  Class isa;
}
+ (id) new;
@end

__attribute__ ((deprecated))
@implementation DeprecatedClass /* { dg-warning "prefix attributes are ignored" } */
+ (id) new { return nil; }
@end

void function (void)
{
  DeprecatedClass *object = [DeprecatedClass new]; /* { dg-warning "is deprecated" } */ 
}
