/* Test warning for comparison non-null address with null pointer constant. */
/* Origin: Pawel Sikora <pluto@agmk.net> */
/* { dg-do compile } */
/* { dg-options "-Waddress" } */
extern void z();

void f() { if ( z ) z(); } /* { dg-warning "always evaluate as" } */
void g() { if ( z != 0 ) z(); } /* { dg-warning "the comparison will always evaluate as 'true'" } */
void h() { if ( z != (void*)0 ) z(); } /* { dg-warning "the comparison will always evaluate as 'true'" } */
