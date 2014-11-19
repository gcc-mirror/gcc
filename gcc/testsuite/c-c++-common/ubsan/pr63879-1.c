/* PR sanitizer/63879 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

struct A
{
  int inode;
} * a;
int b, c;
void
fn1 ()
{
  int d = 0;
  while (b)
    {
      if (a->inode)
        d++;
      a = 0;
    }
  c = d - 1;
  for (; c >= 0; c--)
    ;
}
