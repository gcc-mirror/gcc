/* PR tree-optimization/69172 - ICE in make_ssa_name_fn,
   at tree-ssanames.c:266 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a;

void *
f1 (void)
{
  int *b = &a, *c = &a;
  return __builtin___mempcpy_chk (b, c, sizeof (int), 0);
}

void *
f2 (void)
{
  int *b = &a;
  return __builtin___mempcpy_chk (b, b, sizeof (int), 0);
}

void *
f3 (void)
{
  return __builtin___mempcpy_chk (&a, &a, sizeof (int), 0);
}

void *
f4 (int x)
{
  int *b = &a, *c = &a;
  return __builtin___mempcpy_chk (b, c, x, 0);
}

void *
f5 (int x)
{
  int *b = &a;
  return __builtin___mempcpy_chk (b, b, x, 0);
}

void *
f6 (int x)
{
  return __builtin___mempcpy_chk (&a, &a, x, 0);
}

/* The calls above violate strict aliasing.  Eliminate the -Wrestrict
   warnings they trigger.
  { dg-prune-output "\\\[-Wrestrict]" } */
