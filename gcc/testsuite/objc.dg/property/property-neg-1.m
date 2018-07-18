/* { dg-do compile } */

@interface Bar
{
  int iVar;
}
@property int fooBar;
@end

@implementation Bar
@end /* { dg-warning "incomplete implementation of class .Bar." } */
     /* { dg-warning "method definition for .-setFooBar:. not found" "" { target *-*-* } .-1 } */
     /* { dg-warning "method definition for .-fooBar. not found" "" { target *-*-* } .-2 } */
