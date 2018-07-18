/* PR tree-optimization/82059 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-isolate-paths" } */
/* { dg-skip-if "accessing data memory with program memory address" { avr-*-* } } */

struct a
{
  char b;
  struct a *c;
} d (), f;
void *e;
long g;
void
h ()
{
  struct a *i = 0;
  if (g)
    i = e;
  if (!i)
    d ();
  i->c = &f;
  i->b = *(char *) h;
}
