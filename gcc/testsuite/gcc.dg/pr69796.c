/* PR c/69796 */
/* { dg-do compile } */

struct S s;	/* { dg-error "storage size of 's' isn't known" } */

void
foo ()
{
  s a;	/* { dg-error "expression statement has incomplete type|expected" } */
}
