/* PR preprocessor/60400 */
/* { dg-do compile } */
/* { dg-options "-trigraphs -Wtrigraphs" } */

??=include "pr60400-1.h"
??=include "pr60400-2.h"

/* { dg-warning "trigraph" "" { target *-*-* } 1 } */
/* { dg-warning "trigraph" "" { target *-*-* } 2 } */
/* { dg-warning "trigraph" "" { target *-*-* } 3 } */
/* { dg-warning "trigraph" "" { target *-*-* } 4 } */
/* { dg-warning "trigraph" "" { target *-*-* } 5 } */
/* { dg-warning "trigraph" "" { target *-*-* } 6 } */
