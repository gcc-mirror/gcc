/* { dg-do compile } */

@interface Person 
{
  char *firstName;
}
@property char *firstName;
@end	

@implementation  Person
@dynamic firstName;    /* { dg-line firstName_first } */
@synthesize firstName; /* { dg-error "property .firstName. already specified in .@dynamic." } */
                       /* { dg-message "originally specified here" "" { target *-*-* } firstName_first } */
@end
