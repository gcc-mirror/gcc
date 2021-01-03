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
  double a[];                                 /* { dg-error "instance variable .a. has unknown size" } */
  struct { int x; double y[]; } b;            /* { dg-error "instance variable .b. uses flexible array member" } */
  test_type c;                                /* { dg-error "instance variable .c. uses flexible array member" } */
  test_type d[4];                             /* { dg-error "instance variable .d. uses flexible array member" } */
  union union_type { int x; test_type y; } e; /* { dg-error "instance variable .e. uses flexible array member" } */
}
@end

@implementation Test
@end
