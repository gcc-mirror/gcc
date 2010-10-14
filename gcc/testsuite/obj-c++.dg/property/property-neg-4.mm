/* Property cannot be accessed in class method. */
/* { dg-do compile } */

@interface Person 
{
}
@property char *fullName;
+ (void) testClass;
@end	

@implementation  Person
@property char *fullName;
+ (void) testClass {
	fullName = "MyName"; /* { dg-error "property 'fullName' accessed in class method" } */
			     /* { dg-error "'fullName' was not declared in this scope" "" { target *-*-* } 14 } */
}
@end

