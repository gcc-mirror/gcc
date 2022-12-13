/* Verify that simple indirect calls are inlined even when
   attribute __optimize is used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-inline"  } */

extern void non_existent(int);

static void hooray ()
{
  non_existent (1);
}

__attribute__ ((__optimize__ ("O2")))
static void hiphip (void (*f)())
{
  non_existent (2);
  f ();
}

int test (void)
{
  hiphip (hooray);
  return 0;
}

/* { dg-final { scan-ipa-dump "hooray\[^\\n\]*inline copy in test" "inline" { xfail sparc*-*-* visium-*-* } } } */
/* Missing back-end support for attribute __optimize__ on SPARC and Visium.  */
