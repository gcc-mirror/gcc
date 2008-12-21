/* { dg-do run } */
/* { dg-options "-O2 -mdsp -mgp32" } */

extern void abort (void);
extern void exit (int);

NOMIPS16 void __attribute__ ((noinline))
test1 (int i)
{
  __builtin_mips_wrdsp (i, 63);
}

NOMIPS16 void __attribute__ ((noinline))
test2 ()
{
  long long a = 0;
  __builtin_mips_extpdp (a, 3);
}

NOMIPS16 void __attribute__ ((noinline))
test3 (int i)
{
  long long a = 0;
  __builtin_mips_extpdp (a, i);
}

NOMIPS16 void __attribute__ ((noinline))
test4 ()
{
  long long a = 0;
  int i = 0;
  __builtin_mips_mthlip (a, i);
}

NOMIPS16 int
main ()
{
  int cntl;

  /* Test 1: wrdsp */
  __builtin_mips_wrdsp (0,63);
  test1 (63);
  cntl = __builtin_mips_rddsp (63);
  if (cntl != 63)
    abort ();

  /* Test 2: extpdp */
  __builtin_mips_wrdsp (63,63);
  test2 ();
  cntl = __builtin_mips_rddsp (63);
  if (cntl != 59)
    abort ();

  /* Test 3: extpdpv */
  __builtin_mips_wrdsp (63,63);
  test3 (10);
  cntl = __builtin_mips_rddsp (63);
  if (cntl != 52)
    abort ();

  /* Test 4: mthlip */
  __builtin_mips_wrdsp (8,63);
  test4 ();
  cntl = __builtin_mips_rddsp (63);
  if (cntl != 40)
    abort ();

  exit (0);
}
