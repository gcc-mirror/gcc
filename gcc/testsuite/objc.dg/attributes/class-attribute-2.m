/* { dg-do compile } */

#include <objc/objc.h>

__attribute__ ((deprecated)) 
@interface DeprecatedClass
{
  Class isa;
}
+ (id) new;
@end

__attribute__ ((deprecated))
@implementation DeprecatedClass /* { dg-warning "prefix attributes are ignored for implementations" } */
+ (id) new { return nil; }
@end

void function (void)
{
  DeprecatedClass *object = [DeprecatedClass new]; /* { dg-warning "is deprecated" } */ 
}
