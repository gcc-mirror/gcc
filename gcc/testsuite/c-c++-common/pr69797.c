/* PR c++/69797 */
/* { dg-do compile } */

void
foo () 
{
  __atomic_fetch_add ();	/* { dg-error "too few arguments to function" } */
}
