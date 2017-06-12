/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that a duplicated @implementation for the same class does not
   crash the compiler.  */

@interface Test
{
  Class isa;
}
- (int) test;
@end

@implementation Test
- (int) test
{
  return 4;
}
@end

/* The most likely cause is that the programmer meant this to be a
   category, so check what happens if we have some different methods
   in there.  */
@implementation Test /* { dg-error "reimplementation of class .Test." } */
- (int) test2
{
  return [self test];
}
@end
/* { dg-warning "incomplete implementation" "" { target *-*-* } .-1 } */
/* { dg-warning "not found" "" { target *-*-* } .-2 } */
