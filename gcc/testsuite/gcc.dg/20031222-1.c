/* PR c/9163 */
/* The following test used to ICE after an error message in C99 mode
   because GCC was trying to expand the tree to rtl.  */

/* { dg-do compile } */
/* { dg-options "-std=c99" } */



void f ()
{
	for (; int ; ); /* { dg-error "" } */
}

void foo ()
{
        while (int i); /* { dg-error "" } */
}
