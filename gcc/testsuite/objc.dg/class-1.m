/* Redeclarations of class names.  */
/* { dg-do compile } */

typedef int foo;

@class foo;  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } 4 } */

typedef int bar;

@interface bar
@end  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } 9 } */

int glob;

@implementation glob
@end  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } 15 } */
/* { dg-warning "annot find interface declaration" "" { target *-*-* } 18 } */
