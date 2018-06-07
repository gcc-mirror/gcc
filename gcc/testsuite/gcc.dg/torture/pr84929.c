/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

int a[4];
void fn1()
{
  __UINT64_TYPE__ b = 7818038963515661296;
  for (;; b++)
    a[0xA699ECD2C348A3A0] = a[b];
}
