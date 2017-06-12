/* Test bad warning for anonymous int in structure.  Bug 17189.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

struct Foo { int; }; /* { dg-bogus "unnamed" } */
/* { dg-error "declaration does not declare anything" "int;" { target *-*-* } .-1 } */
/* { dg-error "struct has no members" "no members" { target *-*-* } .-2 } */
