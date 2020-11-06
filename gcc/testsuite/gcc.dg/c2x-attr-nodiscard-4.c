/* Test C2x nodiscard attribute: duplicates (allowed after N2557).  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[nodiscard, __nodiscard__]] int f (void);
[[__nodiscard__, nodiscard("message")]] int g (void);
