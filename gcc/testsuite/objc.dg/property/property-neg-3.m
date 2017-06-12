/* { dg-do compile } */

@interface Person 
{
  char *firstName;
}
@property char *firstName;
@end	

@implementation  Person
@dynamic firstName;    /* { dg-line dynamic_firstName } */
@synthesize firstName; /* { dg-error "property .firstName. already specified in .@dynamic." } */
                       /* { dg-message "originally specified here" "" { target *-*-* } dynamic_firstName } */
@end
