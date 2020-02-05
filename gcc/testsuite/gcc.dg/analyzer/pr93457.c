/* { dg-do compile } */

void
p5 (const void *);

void
s5 (const void *cl)
{
  p5 (&cl[1]); /* { dg-warning "dereferencing 'void \\*' pointer" } */
}
