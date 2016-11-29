/* -fstack-limit- should be ignored without an ICE if not supported.  */
/* { dg-do compile } */
/* { dg-options "-fstack-limit-symbol=_stack_limit -m68000" } */
/* { dg-warning "not supported" "" { target *-*-* } 0 } */

void dummy (void) { }
