/* PR 108375
 * { dg-do compile }
 * { dg-options "" }
 * */

void
f (int a)
{
  typedef int A[a];
  goto x;	/* { dg-error "jump into scope of identifier with variably modified type" } */
  A *p[2];
  x : ;
}


