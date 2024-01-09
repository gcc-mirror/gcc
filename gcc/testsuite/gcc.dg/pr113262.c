/* PR c/113262 */
/* { dg-do compile } */
/* { dg-options "" } */

int [[gnu::copy ("")]] a;	/* { dg-error "'copy' attribute argument cannot be a string" } */

