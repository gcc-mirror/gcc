/* PR c/81795 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++-compat"

struct S { int f; };	/* { dg-bogus "note: originally defined here" } */
typedef int S;		/* { dg-bogus "invalid in C\[+\]\[+\]" } */

typedef int T;		/* { dg-bogus "note: originally defined here" } */
struct T { int f; };    /* { dg-bogus "invalid in C\[+\]\[+\]" } */

#pragma GCC diagnostic pop
