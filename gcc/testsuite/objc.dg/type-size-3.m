/* Reject ivars that use flexible array members.  */
/* Contributed by Nicola Pero  <nicola.pero@meta-innovation.com> */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

typedef struct
{
  unsigned long int a;
  double b[];
} test_type;

@interface Test
{
  test_type c; /* { dg-error "instance variable .c. uses flexible array member" } */
}
@end

@implementation Test
@end
