/* PR c/11995 */
/* The following test used to ICE after an error message
   because GCC was trying to expand the trees to rtl.  */

/* { dg-do compile } */
/* { dg-options "" } */

void f ()
{
 l: int; /* { dg-error "" } */
}
