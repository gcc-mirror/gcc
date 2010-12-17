/* { dg-do compile } */

@interface A
+(void)method: (int)parameter {} /* { dg-error "expected" } */
@end 

@implementation A
+(void)method: (int)parameter
{
  *parameter; /* { dg-error "invalid type argument" } */
}
@end 
