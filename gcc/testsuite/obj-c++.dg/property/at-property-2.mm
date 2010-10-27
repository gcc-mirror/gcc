/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property id name __attribute__((deprecated));
@property id table __attribute__((xxx));        /* { dg-warning ".xxx. attribute directive ignored" } */
@property void function (void);                 /* { dg-error "can.t make .function. into a method" } */
@property typedef int j;                        /* { dg-error "invalid type for property" } */
@end
