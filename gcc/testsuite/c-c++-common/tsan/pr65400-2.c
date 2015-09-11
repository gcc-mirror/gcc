/* PR sanitizer/65400 */
/* { dg-do compile } */

extern void foo (int *);

void
baz4 (int *p)
{
  foo (p);
}
