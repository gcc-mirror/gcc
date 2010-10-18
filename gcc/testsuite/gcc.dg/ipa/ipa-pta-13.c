/* { dg-do link } */
/* { dg-options "-O2 -fipa-pta -fdump-ipa-pta-details -fdump-tree-fre" } */

static int x, y;

static __attribute__((noinline,noclone)) void
local (int *p)
{
  *p = 1;
}

static __attribute__((noinline,noclone)) void
local_address_taken (int *p)
{
  *p = 1;
}

void *anyfn_global;

/* Even though not referenced in this TU we should have added constraints
   for the initializer.  */
/* { dg-final { scan-ipa-dump "ex = &local_address_taken" "pta" } } */
void (*ex)(int *) = local_address_taken;

extern void link_error (void);

int main()
{
  void (*anyfn)(int *) = (void (*)(int *))(__SIZE_TYPE__)anyfn_global;
  /* The following should cause local_address_taken to get &x
     as argument, but not local.  We shouldn't get &x added to
     arbitrary special sub-vars of local_address_taken though,
     a missed optimization currently.
     As local_address_taken escapes the translation unit its
     argument points-to set needs to include ESCAPED and NONLOCAL.
     We shouldn't get the functions sub-vars in the ESCAPED solution
     though, another missed-optimization.  This also causes the functions
     uses to be messed up even further.  */
  /* ???  As we don't expand the ESCAPED solution we either get x printed here
     or not based on the phase of the moon.  */
  /* { dg-final { scan-ipa-dump "local_address_taken.arg0 = { ESCAPED NONLOCAL y x }" "pta" { xfail *-*-* } } } */
  /* { dg-final { scan-ipa-dump "local_address_taken.clobber = { ESCAPED NONLOCAL y x }" "pta" { xfail *-*-* } } } */
  /* { dg-final { scan-ipa-dump "local_address_taken.use = { }" "pta" { xfail *-*-* } } } */
  /* ??? But make sure x really escaped.  */
  /* { dg-final { scan-ipa-dump "ESCAPED = {\[^\n\}\]* x \[^\n\}\]*}" "pta" } } */
  (*anyfn) (&x);
  x = 0;
  local (&y);
  /* Thus we should be able to disambiguate x against the call to local
     and CSE the stored value.  */
  if (x != 0)
    link_error ();
  x = 1;
  local_address_taken (&y);
  /* As we are computing flow- and context-insensitive we may not
     CSE the load of x here.  */
  /* { dg-final { scan-tree-dump " = x;" "fre" } } */
  return x;
}

/* { dg-final { cleanup-ipa-dump "pta" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
