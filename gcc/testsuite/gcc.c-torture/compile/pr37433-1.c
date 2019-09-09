/* { dg-require-effective-target indirect_calls } */

void regex_subst(void)
{
  const void *subst = "";
  (*(void (*)(int))subst) (0);
}

void foobar (void)
{
  int x;
  (*(void (*)(void))&x) ();
}
