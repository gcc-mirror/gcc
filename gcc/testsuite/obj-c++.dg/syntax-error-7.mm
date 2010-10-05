/* { dg-do compile } */

@interface Foo
-(void) someMethod;
@end

@implementation Foo
-(void)
-(void) someMethod /* { dg-error "expected before .-." } */
{
}
@end /* { dg-error "incomplete implementation of class" } */
/* { dg-error "method definition for ..someMethod. not found" "" { target *-*-* } 12 } */
