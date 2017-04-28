/* Test for collision of @interfaces with global vars.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface Foo
@end
float Foo;  /* { dg-error "parse error|syntax error|expected|redeclaration" } */

double Bar; /* { dg-line Bar_decl } */
@interface Bar
@end  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } Bar_decl } */
