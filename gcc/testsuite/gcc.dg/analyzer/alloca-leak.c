/* { dg-require-effective-target alloca } */

void *test (void)
{
  void *ptr = __builtin_alloca (64);
  return ptr;
}
/* TODO: warn about escaping alloca.  */
