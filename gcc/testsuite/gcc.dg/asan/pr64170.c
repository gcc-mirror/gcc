/* PR sanitizer/64170 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address,null" } */

int a, *b, c;
void bar (int);

void
foo (void)
{
  char *d = (char *) b;
  if (d[0] && d[1])
    return;
  if (c)
    a = *(int *) d;
  bar (*(int *) d);
}
