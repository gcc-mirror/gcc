/* Test warning for ordered comparison pointer with null pointer constant. */
/* Tested with no warning option. */
/* { dg-do compile } */
/* { dg-options "" } */
extern void z();
void *p;

void f() {
 if (z >= 0)
   z();
 if (0 >= z)
    z();
 if (p >= (void*)0)
    z();
 if ((void*)0 >= p)
    z();
 if (z >= (void*)0) /* { dg-warning "distinct pointer types lacks a cast" } */
    z();
 if ((void*)0 >=z) /* { dg-warning "distinct pointer types lacks a cast" } */
    z();
}
