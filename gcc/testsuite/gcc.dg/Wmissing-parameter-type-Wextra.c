/* Test -Wmissing-parameter-type is enabled by -Wextra */
/* { dg-do compile } */
/* { dg-options "-Wextra" } */

int foo(bar) { return bar;} /* { dg-warning "type of 'bar' defaults to 'int'" } */


