/* PR preprocessor/60400 */
/* { dg-do compile } */
/* { dg-options "-trigraphs -Wtrigraphs" } */

??=include "pr60400-1.h" /* { dg-warning "trigraph" } */
??=include "pr60400-2.h" /* { dg-warning "trigraph" } */

/* These are line numbers in pr60400-{1,2}.h  Keep them absolute.  */
/* { dg-warning "trigraph" "" { target *-*-* } 1 } */
/* { dg-warning "trigraph" "" { target *-*-* } 2 } */
/* { dg-warning "trigraph" "" { target *-*-* } 3 } */
/* { dg-warning "trigraph" "" { target *-*-* } 4 } */

