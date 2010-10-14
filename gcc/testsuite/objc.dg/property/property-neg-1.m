/* This program checks for proper use of 'readonly' attribute. */
/* { dg-do compile } */

@interface Bar
{
  int iVar;
}
@property (readonly) int FooBar;
@end

@implementation Bar
@property int FooBar; /* { dg-error "property 'FooBar' 'readonly' attribute conflicts with its interface version" } */

@end
