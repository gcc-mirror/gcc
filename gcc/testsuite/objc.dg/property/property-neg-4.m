/* { dg-do compile } */

@interface Person 
{
  char *fullName;
}
@property char *fullName;
+ (void) testClass;
@end	


@implementation  Person
@synthesize fullName;
+ (void) testClass {
  self.fullName = "MyName"; /* { dg-error "request for member .fullName." } */
}
@end
