/* PR c/93573 */
/* { dg-do compile } */
/* { dg-options "" } */

int f[100.0];	/* { dg-error "has non-integer type" } */
                /* { dg-bogus "variably modified .f. at file scope" "" { target *-*-* } .-1 } */
