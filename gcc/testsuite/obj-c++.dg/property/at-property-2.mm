/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property int name __attribute__((deprecated));
@property int table __attribute__((xxx));       /* { dg-warning ".xxx. attribute directive ignored" } */
@property void function (void);                 /* { dg-error "declaration of function .function. in invalid context" } */
@property typedef int j;                        /*  { dg-error "invalid type for property" } */
@end
