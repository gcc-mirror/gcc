/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

const void *s0 = R"ouch[]ouCh";	/* { dg-error "expected expression at end of input" } */
	/* { dg-error "unterminated raw string" "" { target *-*-* } 4 } */
