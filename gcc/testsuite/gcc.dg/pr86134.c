/* { dg-do compile } */
/* { dg-options "-Wall -Werror -Wno-error=main -Wno-foobar" } */

void main() {} /* { dg-warning "return type" } */

/* { dg-message "unrecognized command-line option" "" { target *-*-* } 0 } */
