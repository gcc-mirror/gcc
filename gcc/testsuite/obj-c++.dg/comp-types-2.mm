/* Test various ObjC types assignments and comparisons.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */

#include <objc/objc.h>

@protocol MyProtocol
- (void) foo;
@end

@interface MyClass
@end

@interface MyOtherClass <MyProtocol>
- (void) foo;
@end

int main()
{
  id obj = nil;
  id<MyProtocol> obj_p = nil;
  MyClass *obj_c = nil;
  MyOtherClass *obj_cp = nil;
  Class obj_C = Nil;

  /* Assigning to an 'id' variable should never
     generate a warning.  */
  obj = obj_p;  /* Ok  */
  obj = obj_c;  /* Ok  */
  obj = obj_cp; /* Ok  */
  obj = obj_C;  /* Ok  */
  
  /* Assigning to a 'MyClass *' variable should always generate a
     warning, unless done from an 'id'.  */
  obj_c = obj;    /* Ok */
  obj_c = obj_p;  /* { dg-warning "distinct Objective\\-C type" } */
  obj_c = obj_cp; /* { dg-warning "distinct Objective\\-C type" } */
  obj_c = obj_C;  /* { dg-warning "distinct Objective\\-C type" } */

  /* Assigning to an 'id<MyProtocol>' variable should generate a
     warning if done from a 'MyClass *' (which doesn't implement
     MyProtocol), but not from an 'id' or from a 'MyOtherClass *'
     (which implements MyProtocol).  */
  obj_p = obj;    /* Ok */
  obj_p = obj_c;  /* { dg-warning "does not implement" } */
  obj_p = obj_cp; /* Ok  */
  obj_p = obj_C;  /* { dg-warning "distinct Objective\\-C type" } */

  /* Assigning to a 'MyOtherClass *' variable should always generate
     a warning, unless done from an 'id' or an 'id<MyProtocol>' (since
     MyOtherClass implements MyProtocol).  */
  obj_cp = obj;    /* Ok */
  obj_cp = obj_c;  /* { dg-warning "distinct Objective\\-C type" } */
  obj_cp = obj_p;  /* Ok */
  obj_cp = obj_C;  /* { dg-warning "distinct Objective\\-C type" } */

  /* Any comparison involving an 'id' must be without warnings.  */
  if (obj == obj_p) ;  /* Ok  */ /*Bogus warning here in 2.95.4*/
  if (obj_p == obj) ;  /* Ok  */
  if (obj == obj_c) ;  /* Ok  */
  if (obj_c == obj) ;  /* Ok  */
  if (obj == obj_cp) ; /* Ok  */
  if (obj_cp == obj) ; /* Ok  */
  if (obj == obj_C) ;  /* Ok  */
  if (obj_C == obj) ;  /* Ok  */

  /* Any comparison between 'MyClass *' and anything which is not an 'id'
     must generate a warning.  */
  if (obj_c == obj_p) ; /* { dg-warning "lacks a cast" } */
  if (obj_p == obj_c) ; /* { dg-warning "lacks a cast" } */
  if (obj_c == obj_cp) ; /* { dg-warning "lacks a cast" } */
  if (obj_cp == obj_c) ; /* { dg-warning "lacks a cast" } */
  if (obj_c == obj_C) ;  /* { dg-warning "lacks a cast" } */
  if (obj_C == obj_c) ;  /* { dg-warning "lacks a cast" } */

  /* Any comparison between 'MyOtherClass *' (which implements
     MyProtocol) and an 'id' implementing MyProtocol are Ok.  */
  if (obj_cp == obj_p) ; /* Ok */
  if (obj_p == obj_cp) ; /* Ok */


  if (obj_p == obj_C) ; /* { dg-warning "lacks a cast" } */
  if (obj_C == obj_p) ; /* { dg-warning "lacks a cast" } */
  if (obj_cp == obj_C) ; /* { dg-warning "lacks a cast" } */
  if (obj_C == obj_cp) ; /* { dg-warning "lacks a cast" } */

  return 0;
}
