/* Allow ivars that are pointers to structs with an unknown size.  */
/* Contributed by Nicola Pero  <nicola.pero@meta-innovation.com> */
/* PR objc/47832 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

typedef struct
{
  unsigned long int a;
  double b[];
} test_type;

@interface Test
{
  /* These are all fine.  */
  double *a;
  struct { int x; double y[]; } *b;
  test_type *c;
  union union_type { int x; test_type y; } *d;
}
@end

@implementation Test
@end
