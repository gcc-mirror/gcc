/* This test checks that local aliases behave sanely.  This is necessary for code correctness
   of aliases introduced by ipa-visibility pass.

   If this test fails either aliases needs to be disabled on given target on aliases with
   proper semantic needs to be implemented.  This is problem with e.g. AIX .set pseudo-op
   that implementes alias syntactically (by substituting in assembler) rather as alternative
   symbol defined on a target's location.  */

/* { dg-do run }
   { dg-options "-Wstrict-aliasing=2 -fstrict-aliasing" } 
   { dg-require-alias "" }
   { dg-additional-sources "localalias-2.c" } */
extern void abort (void);
extern void tt (void);
extern int test2count;
int testcount;
__attribute__ ((weak,noinline))
void test(void)
{
  testcount++;
}
__attribute ((alias("test")))
static void test2(void);

int main()
{
  test2();
  /* This call must bind locally.  */
  if (!testcount)
    abort ();
  test();
  /* Depending on linker choice, this one may bind locally
     or to the other unit.  */
  if (!testcount && !test2count)
    abort();
  tt();

  if ((testcount != 1 || test2count != 3)
      && (testcount != 3 || test2count != 1))
    abort ();
  return 0;
}
