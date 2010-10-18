/* Test @class.  */
/* { dg-do compile } */

@class Object; /* Ok */

@class Object, ;                            /* { dg-error "expected identifier" } */
@class Object, ;                            /* { dg-error "expected identifier" } */
@class Object, AnotherObject, ;             /* { dg-error "expected identifier" } */
@class Object, AnotherObject, TestObject ;  /* Ok */

@class Object                               /* { dg-error "expected .;." } */
