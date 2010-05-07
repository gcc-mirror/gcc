/* Test warning for ordered comparison pointer with null pointer constant. */
/* Test with -Wextra. */
/* { dg-do compile } */
/* { dg-options "-Wextra" } */
extern void z();
void *p;

void f() {
 if (z >= 0) /* { dg-warning "ordered comparison of pointer with" } */
   z();
 if (0 >= z) /* { dg-warning "ordered comparison of pointer with" } */
    z();
 if (p >= (void*)0) /* { dg-warning "ordered comparison of pointer with null pointer" } */
    z();
 if ((void*)0 >= p) /* { dg-warning "ordered comparison of pointer with null pointer" } */
    z();
 if (z >= (void*)0) /* { dg-warning "distinct pointer types lacks a cast" } */
    z();
 if ((void*)0 >=z) /* { dg-warning "distinct pointer types lacks a cast" } */
    z();
}
