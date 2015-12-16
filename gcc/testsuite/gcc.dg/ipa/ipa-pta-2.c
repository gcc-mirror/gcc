/* { dg-do compile } */
/* { dg-options "-O -fipa-pta -fdump-ipa-pta2-details" } */

int (*fn)(int *);

static int __attribute__((noinline,noclone))
foo (int *p)
{
  return *p;
}

extern void bar (void);

int main()
{
  fn = foo;
  bar ();
  return 0;
}

/* Make sure that when a local function escapes its argument points-to sets
   are properly adjusted.  */

/* { dg-final { scan-ipa-dump "foo.arg0 = { ESCAPED NONLOCAL }" "pta2" } } */
