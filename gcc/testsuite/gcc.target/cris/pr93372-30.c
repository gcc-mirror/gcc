/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#define t int
#define op >>
#define f ashrtf

/* If we don't disable g, it will degenerate into a test of the
   input. */
#define do_g 0

#include "pr93372-16.c"
#undef do_g

#undef t
#define t unsigned int
#undef f
#undef g
#define f lshrtf
#define g lshrtg
#include "pr93372-16.c"

#undef f
#undef g
#undef op
#define op <<
#define f shlf
#define g shlg
#include "pr93372-16.c"
