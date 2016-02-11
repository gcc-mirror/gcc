/* This test checks that local aliases behave sanely.  This is necessary for code correctness
   of aliases introduced by ipa-visibility pass.

   This test expose weird behavior of AIX's .set pseudo-op where the global symbol is created,
   but all uses of the alias are syntactically replaced by uses of the target.  This means that
   both counters are increased to 2.  */

/* { dg-do run }
   { dg-options "-O2" } 
   { dg-require-alias "" }
   { dg-additional-sources "globalalias-2.c" } */
extern int test2count;
extern void abort (void);
extern void tt ();
int testcount;
static
void test(void)
{
  testcount++;
}
__attribute__ ((weak,noinline))
__attribute ((alias("test")))
void test2(void);

int main()
{
  test();
  /* This call must bind locally.  */
  if (!testcount)
    abort ();
  test2();
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
