/* Redeclarations of class names.  */
/* { dg-do compile } */

typedef int foo; /* { dg-line foo_def } */

@class foo;  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } foo_def } */

typedef int bar; /* { dg-line bar_def } */

@interface bar
@end  /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-error "previous declaration of" "" { target *-*-* } bar_def } */

int glob; /* { dg-line glob_def } */

@implementation glob
@end /* { dg-line glob_impl_end } */
/* { dg-error "redeclared as different kind of symbol" "" { target *-*-* } glob_impl_end } */
/* { dg-error "previous declaration of" "" { target *-*-* } glob_def } */
/* { dg-warning "annot find interface declaration" "" { target *-*-* } glob_impl_end } */
