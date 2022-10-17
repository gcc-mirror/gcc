/* PR tree-optimization/102009 */
/* { dg-do compile } */

void *realloc ();	/* { dg-message "declared here" } */

void *
foo (void *p)
{
  return realloc (p);	/* { dg-warning "too few arguments to built-in function 'realloc' expecting " } */
}
