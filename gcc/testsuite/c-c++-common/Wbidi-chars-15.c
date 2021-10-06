/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=unpaired" } */
/* Test unpaired bidi control chars in multiline comments.  */

/*
 * LRE‪ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * RLE‫ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * LRO‭ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * RLO‮ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * LRI⁦ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * RLI⁧ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/*
 * FSI⁨ end
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/* LRE‪
   PDF‬ */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */
/* FSI⁨
   PDI⁩ */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */

/* LRE<‪>
 *
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-3 } */

/*
 * LRE<‪>
 */
/* { dg-warning "unpaired" "" { target *-*-* } .-2 } */

/*
 *
 * LRE<‪> */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */

/* RLI<⁧> */ /* PDI<⁩> */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
/* LRE<‪> */ /* PDF<‬> */
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
