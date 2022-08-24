/* PR sanitizer/104449 */
/* { dg-do compile } */
/* { dg-options "-fexceptions -fsanitize=address -fstack-check=generic" } */

void bar (int *);

void
foo (void)
{
  int a[16];
  bar (a);
}
