/* Test assignments and comparisons between protocols (obscure case).  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */

#include <objc/objc.h>

@protocol MyProtocolA
- (void) methodA;
@end

@protocol MyProtocolB
- (void) methodB;
@end

@protocol MyProtocolAB <MyProtocolA, MyProtocolB>
@end

@protocol MyProtocolAC <MyProtocolA>
- (void) methodC;
@end

int main()
{
  id<MyProtocolA> obj_a = nil;
  id<MyProtocolB> obj_b = nil;
  id<MyProtocolAB> obj_ab = nil;
  id<MyProtocolAC> obj_ac = nil;

  obj_a = obj_b;  /* { dg-warning "does not conform" } */
  obj_a = obj_ab; /* Ok */
  obj_a = obj_ac; /* Ok */
  
  obj_b = obj_a;  /* { dg-warning "does not conform" } */
  obj_b = obj_ab; /* Ok */
  obj_b = obj_ac; /* { dg-warning "does not conform" } */
  
  obj_ab = obj_a;  /* { dg-warning "does not conform" } */
  obj_ab = obj_b;  /* { dg-warning "does not conform" } */
  obj_ab = obj_ac; /* { dg-warning "does not conform" } */
  
  obj_ac = obj_a;  /* { dg-warning "does not conform" } */
  obj_ac = obj_b;  /* { dg-warning "does not conform" } */
  obj_ac = obj_ab; /* { dg-warning "does not conform" } */

  if (obj_a == obj_b) ; /* { dg-warning "lacks a cast" } */
  if (obj_b == obj_a) ; /* { dg-warning "lacks a cast" } */

  if (obj_a == obj_ab) ; /* Ok */
  if (obj_ab == obj_a) ; /* Ok */ /* Spurious 2.95.4 warning here */

  if (obj_a == obj_ac) ; /* Ok */ 
  if (obj_ac == obj_a) ; /* Ok */ /* Spurious 2.95.4 warning here */

  if (obj_b == obj_ab) ; /* Ok */ 
  if (obj_ab == obj_b) ; /* Ok */ /* Spurious 2.95.4 warning here */

  if (obj_b == obj_ac) ; /* { dg-warning "lacks a cast" } */ 
  if (obj_ac == obj_b) ; /* { dg-warning "lacks a cast" } */ 

  if (obj_ab == obj_ac) ; /* { dg-warning "lacks a cast" } */ 
  if (obj_ac == obj_ab) ; /* { dg-warning "lacks a cast" } */ 

  return 0;
}
