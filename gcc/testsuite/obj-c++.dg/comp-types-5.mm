/* Test errors for assignments and comparisons between ObjC and C++ types.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

#include <objc/objc.h>

/* The NeXT runtime headers do not define NULL.  */
#ifndef NULL
#define NULL ((void *)0)
#endif

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
  int *j = (int *)NULL;

  /* These should all generate warnings.  */
  
  obj = i; /* { dg-error "invalid conversion" } */
  obj = j; /* { dg-error "cannot convert" } */

  obj_p = i; /* { dg-error "invalid conversion" } */
  obj_p = j; /* { dg-error "cannot convert" } */
  
  obj_c = i; /* { dg-error "invalid conversion" } */
  obj_c = j; /* { dg-error "cannot convert" } */

  obj_C = i; /* { dg-error "invalid conversion" } */
  obj_C = j; /* { dg-error "cannot convert" } */
  
  i = obj;   /* { dg-error "invalid conversion" } */
  i = obj_p; /* { dg-error "invalid conversion" } */
  i = obj_c; /* { dg-error "invalid conversion" } */
  i = obj_C; /* { dg-error "invalid conversion" } */
  
  j = obj;   /* { dg-error "cannot convert" } */
  j = obj_p; /* { dg-error "cannot convert" } */
  j = obj_c; /* { dg-error "cannot convert" } */
  j = obj_C; /* { dg-error "cannot convert" } */
  
  if (obj == i) ; /* { dg-error "comparison between pointer and integer" } */
  if (i == obj) ; /* { dg-error "comparison between pointer and integer" } */
  if (obj == j) ; /* { dg-error "lacks a cast" } */
  if (j == obj) ; /* { dg-error "lacks a cast" } */

  if (obj_c == i) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (i == obj_c) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (obj_c == j) ; /* { dg-error "lacks a cast" } */
  if (j == obj_c) ; /* { dg-error "lacks a cast" } */

  if (obj_p == i) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (i == obj_p) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (obj_p == j) ; /* { dg-error "lacks a cast" } */
  if (j == obj_p) ; /* { dg-error "lacks a cast" } */

  if (obj_C == i) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (i == obj_C) ; /*{ dg-error "comparison between pointer and integer" }*/
  if (obj_C == j) ; /* { dg-error "lacks a cast" } */
  if (j == obj_C) ; /* { dg-error "lacks a cast" } */

  return 0;
}
