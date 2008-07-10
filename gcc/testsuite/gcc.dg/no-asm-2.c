/* { dg-do compile } */
/* { dg-options "-std=gnu89 -fno-asm" } */

/* Verify that these GNU extensions are not recognized as keywords
   when using -fno-asm in GNU89 mode.  */

int asm;	/* { dg-bogus "before .asm." } */
int inline;	/* { dg-bogus "empty declaration" } */
int typeof;	/* { dg-bogus "before .typeof." } */
