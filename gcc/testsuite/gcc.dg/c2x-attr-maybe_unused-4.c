/* Test C2x maybe_unused attribute: duplicates (allowed after N2557).  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[maybe_unused, __maybe_unused__]] int a;
[[__maybe_unused__, maybe_unused]] int b;
int c [[maybe_unused, maybe_unused]];
[[maybe_unused, maybe_unused]];
/* { dg-error "ignored" "ignored" { target *-*-* } .-1 } */

[[maybe_unused]] [[maybe_unused]] int d [[maybe_unused]] [[maybe_unused]];
