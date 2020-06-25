/* PR c/95580 */
/* { dg-do compile } */
/* { dg-options "-O1 -W -fno-tree-dce" } */

void bar (void);

void
foo (int x)
{
  if (x == 0)
    {
      void *p = __builtin_malloc (4);
      ((char *)p)[1] ^= 1;	/* { dg-warning "may be used uninitialized" } */
    }
  bar ();
}
