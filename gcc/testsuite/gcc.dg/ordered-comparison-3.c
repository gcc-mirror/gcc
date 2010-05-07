/* Test warning for ordered comparison pointer with null pointer constant. */
/* Test with -pedantic-errors. */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */
extern void z();
void *p;

void f() {
 if ( z >= 0 ) /* { dg-error "ordered comparison of pointer with" } */
   z();
 if ( 0 >= z) /* { dg-error "ordered comparison of pointer with" } */
    z();
 if ( p >= (void*)0 )
    z();
 if ( (void*)0 >= p)
    z();
 if (z >= (void*)0) /* { dg-error "distinct pointer types lacks a cast" } */
    z();
 if ((void*)0 >=z) /* { dg-error "distinct pointer types lacks a cast" } */
    z(); 
}
