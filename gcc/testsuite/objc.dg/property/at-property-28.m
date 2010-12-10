/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* Test errors when extending a property in a class extension.  */

#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
}
@property (readonly, retain)    id property1; /* { dg-message "originally specified here" } */
@property (readonly)           int property2; /* { dg-message "originally specified here" } */
@property (readonly, getter=y) int property3; /* { dg-message "originally specified here" } */
@property (readonly)           int property4; /* Ok */
@property (readonly)           int property5; /* { dg-message "originally specified here" } */
@end

@interface MyRootClass ()
@property (readwrite, copy)       id property1; /* { dg-warning "assign semantics attributes of property .property1. conflict with previous declaration" } */
@property (readwrite, nonatomic) int property2; /* { dg-warning ".nonatomic. attribute of property .property2. conflicts with previous declaration" } */
@property (readwrite, getter=x)  int property3; /* { dg-warning ".getter. attribute of property .property3. conflicts with previous declaration" } */
@property (readwrite)            int property4; /* Ok */
@property (readwrite)          float property5; /* { dg-warning "type of property .property5. conflicts with previous declaration" } */
@end



