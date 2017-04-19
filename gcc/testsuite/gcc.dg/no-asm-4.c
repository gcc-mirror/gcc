/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fno-asm" } */

/* Verify that the GNU extensions asm and typeof are not recognized as
   keywords when using -fno-asm in GNU89 mode, but that inline (which
   is a keyword in C99 but not C89) is recognized.  */

int asm;	/* { dg-bogus "before .asm." } */
int inline;	/* { dg-warning "empty declaration" } */
/* { dg-error "empty declaration" "" { target *-*-* } .-1 } */
int typeof;	/* { dg-bogus "before .typeof." } */
