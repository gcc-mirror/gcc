/* Test C2x deprecated attribute: duplicates.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[deprecated, __deprecated__]] int a; /* { dg-error "can appear at most once" } */
[[__deprecated__, deprecated("message")]] int b; /* { dg-error "can appear at most once" } */
int c [[deprecated("message"), deprecated]]; /* { dg-error "can appear at most once" } */
[[deprecated, deprecated]]; /* { dg-error "can appear at most once" } */
/* { dg-error "ignored" "ignored" { target *-*-* } .-1 } */

/* Separate attribute lists in the same attribute specifier sequence,
   with the same attribute in them, are OK.  */
[[deprecated]] [[deprecated]] int d [[deprecated]] [[deprecated]];
