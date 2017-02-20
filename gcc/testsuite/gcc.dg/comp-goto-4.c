/* PR middle-end/79537 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

void
f (void)
{
L:
  *&&L;
}

void
f2 (void)
{
   void *p;
L:
   p = &&L;
   *p; /* { dg-warning "dereferencing 'void \\*' pointer" } */
}
