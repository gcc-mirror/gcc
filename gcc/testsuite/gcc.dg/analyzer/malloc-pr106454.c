/* Regression test for leak false positive (PR analyzer/106454).  */

void c (void);

struct a
{
  char **args;
};

void
foo (struct a *d)
{
  c ();
  d->args[0] = __builtin_malloc(42); /* { dg-bogus "leak" "PR analyzer/106454" } */
}
