/* Test that we don't ICE when issuing a -Wselector warning.  */
/* { dg-options "-Wselector" } */
/* { dg-do compile } */

#include <objc/objc.h>

@interface Foo
@end
@implementation Foo
-(void) foo
{
  SEL a;
  a = @selector(b1ar);
}
@end
/* { dg-warning "creating selector for nonexistent method .b1ar." "" { target *-*-* } 0 } */

