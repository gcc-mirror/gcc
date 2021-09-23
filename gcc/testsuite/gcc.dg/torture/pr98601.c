/* PR rtl-optimization/98601 */
/* { dg-do compile } */

void
foo (void *p)
{
  asm ("" : "=m" (*p));			/* { dg-warning "dereferencing 'void \\*' pointer" } */
}

void
bar (void *p)
{
  asm volatile ("" : : "m" (*p));	/* { dg-warning "dereferencing 'void \\*' pointer" } */
}
