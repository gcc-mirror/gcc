/* PR c/38957 */
/* { dg-do compile } */

char *
foo (void)
{
  char a[2];
  return a+1;	/* { dg-warning "returns address of local variable" } */
}

char *
bar (void)
{
  char a[2];
  return a;	/* { dg-warning "returns address of local variable" } */
}
