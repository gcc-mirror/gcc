/* Test that we don't ICE when issuing a -Wselector warning.  */
/* { dg-options "-Wselector -fgnu-runtime" } */
/* { dg-do compile } */

#include <objc/Object.h>

@interface Foo
@end
@implementation Foo
-(void) foo
{
  SEL a;
  a = @selector(b1ar);
}
@end /* { dg-warning "creating selector for nonexistent method .b1ar." } */

