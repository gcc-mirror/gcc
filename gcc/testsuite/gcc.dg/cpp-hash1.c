/* Test for proper handling of # in object-like macros.
   From Linux kernel.  */
/* { dg-do preprocess } */

#define FIXUP	.section ".fixup",#alloc,#execinstr

FIXUP

/* { dg-bogus "not followed by" "object-like #" { target *-*-* } 3 } */
