/* Test for collision of @interfaces with global vars.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface Foo
@end
float Foo;  /* { dg-error "(parse|syntax) error" } */

double Bar;
@interface Bar
@end  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } 9 } */
