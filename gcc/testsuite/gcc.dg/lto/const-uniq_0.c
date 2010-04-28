/* The 3 constant initializers should be uniquized.  */

/* { dg-lto-do run } */
/* { dg-lto-options {{-Os -flto} {-Os -fwhopr} } } */

int lookup1 (int i)
{
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  return a[i];
}

int lookup2 (int i)
{
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  return a[i+1];
}
