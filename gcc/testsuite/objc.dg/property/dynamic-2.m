/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

@dynamic isa;  /* { dg-error ".@dynamic. not in @implementation context" } */

@interface Test : MyRootClass
{
  int v1;
}
@end
@implementation Test
@end


@interface Test (Category)
@property int v1;
@end
@implementation Test (Category)
@dynamic v1;
@end


@interface AnotherTest : MyRootClass
{
}
@property int one;
@end

@implementation AnotherTest
@dynamic one;
@dynamic one; /* { dg-error "property .one. already specified in .@dynamic." } */
              /* { dg-message "originally specified here" "" { target *-*-* } 41 } */
@dynamic three; /* { dg-error "no declaration of property .three. found in the interface" } */
@end
