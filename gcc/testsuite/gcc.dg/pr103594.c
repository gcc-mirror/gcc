/* { dg-do compile { target fpic } } */
/* { dg-options "-O1 -fpic" } */

int regex_subst(void)
{
  const void *subst = "";
  return (*(int (*)(int))subst) (0);
}

int foobar (void)
{
  int x;
  return (*(int (*)(void))&x) ();
}
