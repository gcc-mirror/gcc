/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property id name __attribute__((deprecated));
@property id table __attribute__((xxx));        /* { dg-warning ".xxx. attribute directive ignored" } */
@property void function (void);                 /* { dg-error "declared as a function" } */
@property typedef int j;                        /* { dg-error "expected" } */
@end
