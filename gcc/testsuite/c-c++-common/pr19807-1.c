/* { dg-do link } */

extern void link_error(void);
int main()
{
  int a[4];
  if (&a[2]-1 != &a[1])
    link_error();
  return 0;
}
