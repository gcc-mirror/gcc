/* { dg-do compile } */

@interface Person 
{
  char *firstName;
}
@property char *firstName;
@end	

@implementation  Person
@dynamic firstName;
/* FIXME - there is a problem with the testuite in running the following test.  The compiler
   generates the messages, but the testsuite still complains.  */
/*@synthesize firstName;*/ /*  dg-error "property .firstName. already specified in .@dynamic."  */
                       /*  dg-message "originally specified here" "" { target *-*-* } 11  */
@end
