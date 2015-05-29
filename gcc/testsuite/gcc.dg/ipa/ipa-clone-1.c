/* PR ipa/64693 */
/* { dg-do compile } */
/* { dg-require-named-sections "" } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp"  } */

static int __attribute__ ((noinline, section ("test_section")))
foo (int arg)
{
  return 7 * arg;
}

int
bar (int arg)
{
  return foo (5);
}

/* { dg-final { scan-assembler "test_section" } } */
/* { dg-final { scan-ipa-dump "Creating a specialized node of foo" "cp" } } */
