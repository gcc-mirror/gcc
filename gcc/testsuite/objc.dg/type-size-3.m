/* Reject ivars with an unknown size.  */
/* Contributed by Nicola Pero  <nicola.pero@meta-innovation.com> */
/* { dg-do compile } */

typedef struct
{
  unsigned long int a;
  double b[];
} test_type;

@interface Test
{
  test_type c;
}
@end

@implementation Test
@end /* { dg-error "instance variable has unknown size" } */
