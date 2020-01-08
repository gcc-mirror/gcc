/* Test C2x maybe_unused attribute: duplicates.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[maybe_unused, __maybe_unused__]] int a; /* { dg-error "can appear at most once" } */
[[__maybe_unused__, maybe_unused]] int b; /* { dg-error "can appear at most once" } */
int c [[maybe_unused, maybe_unused]]; /* { dg-error "can appear at most once" } */
[[maybe_unused, maybe_unused]]; /* { dg-error "can appear at most once" } */
/* { dg-error "ignored" "ignored" { target *-*-* } .-1 } */

/* Separate attribute lists in the same attribute specifier sequence,
   with the same attribute in them, are OK.  */
[[maybe_unused]] [[maybe_unused]] int d [[maybe_unused]] [[maybe_unused]];
