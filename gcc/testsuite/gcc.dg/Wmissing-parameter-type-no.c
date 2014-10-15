/* Test that we can disable -Wmissing-parameter-type */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -Wno-missing-parameter-type -std=gnu89" } */

int foo(bar) { return bar;} /* { dg-bogus "type of 'bar' defaults to 'int'" } */


