/* { dg-do compile } */

@interface Foo
-(void) someMethod;
@end

@implementation Foo
-(void)
-(void) someMethod /* { dg-error "expected before .-." } */
{
}
@end /* { dg-warning "incomplete implementation of class" } */
/* { dg-warning "method definition for ..someMethod. not found" "" { target *-*-* } 12 } */
