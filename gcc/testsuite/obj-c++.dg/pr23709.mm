/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

@interface A
+(void)method: (int)parameter {} /* { dg-error "expected" } */
@end 

@implementation A
+(void)method: (int)parameter
{
  *parameter; /* { dg-error "invalid type argument" } */
}
@end
