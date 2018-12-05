/* { dg-do link } */
/* { dg-options "-O" } */

extern void link_error(void);
int i;
int main()
{
  int a[4];
  if ((char*)&a[1] + 4*i + 4 != (char*)&a[i+2])
    link_error();
  return 0;
}
