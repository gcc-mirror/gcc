/* Test warnings for assignments and comparisons between ObjC and C types.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
#include <objc/objc.h>

@protocol MyProtocol
- (void) method;
@end

@interface MyClass
@end

int main()
{
  id obj = nil;
  id <MyProtocol> obj_p = nil;
  MyClass *obj_c = nil;
  Class obj_C = Nil;
  
  int i = 0;
  int *j = NULL;

  /* These should all generate warnings.  */
  
  obj = i; /* { dg-warning "pointer from integer without a cast" } */
  obj = j; /* { dg-warning "incompatible pointer type" } */

  obj_p = i; /* { dg-warning "pointer from integer without a cast" } */
  obj_p = j; /* { dg-warning "incompatible pointer type" } */
  
  obj_c = i; /* { dg-warning "pointer from integer without a cast" } */
  obj_c = j; /* { dg-warning "incompatible pointer type" } */

  obj_C = i; /* { dg-warning "pointer from integer without a cast" } */
  obj_C = j; /* { dg-warning "incompatible pointer type" } */
  
  i = obj;   /* { dg-warning "integer from pointer without a cast" } */
  i = obj_p; /* { dg-warning "integer from pointer without a cast" } */
  i = obj_c; /* { dg-warning "integer from pointer without a cast" } */
  i = obj_C; /* { dg-warning "integer from pointer without a cast" } */
  
  j = obj;   /* { dg-warning "incompatible pointer type" } */
  j = obj_p; /* { dg-warning "incompatible pointer type" } */
  j = obj_c; /* { dg-warning "incompatible pointer type" } */
  j = obj_C; /* { dg-warning "incompatible pointer type" } */
  
  if (obj == i) ; /* { dg-warning "comparison between pointer and integer" } */
  if (i == obj) ; /* { dg-warning "comparison between pointer and integer" } */
  if (obj == j) ; /* { dg-warning "lacks a cast" } */
  if (j == obj) ; /* { dg-warning "lacks a cast" } */

  if (obj_c == i) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (i == obj_c) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (obj_c == j) ; /* { dg-warning "lacks a cast" } */
  if (j == obj_c) ; /* { dg-warning "lacks a cast" } */

  if (obj_p == i) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (i == obj_p) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (obj_p == j) ; /* { dg-warning "lacks a cast" } */
  if (j == obj_p) ; /* { dg-warning "lacks a cast" } */

  if (obj_C == i) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (i == obj_C) ; /*{ dg-warning "comparison between pointer and integer" }*/
  if (obj_C == j) ; /* { dg-warning "lacks a cast" } */
  if (j == obj_C) ; /* { dg-warning "lacks a cast" } */

  return 0;
}
