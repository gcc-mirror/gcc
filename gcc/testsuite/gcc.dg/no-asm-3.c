/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Verify that these GNU extensions are recognized as keywords in
   GNU99 mode.  */

int asm;	/* { dg-error "before .asm." } */
int inline;	/* { dg-warning "empty declaration" } */
/* { dg-error "empty declaration" "" { target *-*-* } .-1 } */
int typeof;	/* { dg-error "before .typeof." } */
