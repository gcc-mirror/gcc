/* { dg-do compile } */
/* { dg-options "-Os" } */

void foo (void);
int a, b, c;
int
main (void)
{
  int j;
  for (; c;)
    a = b;
  for (; j;)
    foo ();
}
