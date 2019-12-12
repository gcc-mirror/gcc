/* { dg-require-effective-target indirect_calls } */

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
