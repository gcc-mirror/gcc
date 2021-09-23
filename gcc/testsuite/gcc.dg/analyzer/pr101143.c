/* { dg-additional-options "-Wno-builtin-declaration-mismatch" } */

extern void *malloc (unsigned int);
extern void *alloca (unsigned int);
extern void unknown_fn (void *);

void *
test_malloc (void)
{
  return malloc (sizeof (int));
}

void *
test_alloca (void)
{
  void *p = alloca (sizeof (int));
  unknown_fn (p);
}
