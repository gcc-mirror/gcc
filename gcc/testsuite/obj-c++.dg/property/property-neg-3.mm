/* Property name cannot match the ivar name. */
/* { dg-do compile } */
/* Suppress warnings for incomplete class definition etc. */
/* { dg-options "-w" } */

@interface Person 
{
  char *firstName;
}
@property char *firstName; /* { dg-error "property 'firstName' may not have the same name as an ivar in the class" } */
@end	

@implementation  Person
@end 
