/* { dg-do link } */
/* { dg-options "-O" } */

extern void link_error(void);
int i;
int main()
{
  int a[4];
  if (&a[1] + i + 1 != &a[i+2])
    link_error();
  return 0;
}
