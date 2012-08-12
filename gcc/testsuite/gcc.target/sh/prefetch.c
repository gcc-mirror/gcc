/* Testcase to check generation of a SH4 and SH2A operand cache prefetch
   instruction PREF @Rm.  */
/* { dg-do assemble {target sh*-*-*}}  */
/* { dg-options "-O0" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" "-m3*" "-m4*" } }  */
/* { dg-final { scan-assembler "pref"} }  */

void
opt (void)
{
  int *p, wk;
  int data[100];

  /* data prefetch , instructions hit the cache. */

  __builtin_prefetch (&data[0], 0, 0);
  __builtin_prefetch (&data[0], 0, 1);
  __builtin_prefetch (&data[0], 0, 2);
  __builtin_prefetch (&data[0], 0, 3);
  __builtin_prefetch (&data[0], 1, 0);
  __builtin_prefetch (&data[0], 1, 1);
  __builtin_prefetch (&data[0], 1, 2);
  __builtin_prefetch (&data[0], 1, 3);


  for (p = &data[0]; p < &data[9]; p++)
    {
      if (*p > *(p + 1))
        {
          wk = *p;
          *p = *(p + 1);
          *(p + 1) = wk;
        }
    }
}
