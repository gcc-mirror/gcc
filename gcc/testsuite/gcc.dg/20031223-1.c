/* PR c/11995 */
/* The following test used to ICE after an error message
   because GCC was trying to expand the trees to rtl.  */

/* { dg-do compile } */
/* { dg-options "-std=c17 -pedantic-errors" } */

void f ()
{
 l: int; /* { dg-error "a label can only be part of a statement and a declaration is not a statement" "not stmt" } */
 /* { dg-error "useless type name in empty declaration" "type name" { target *-*-* } .-1 } */
}
