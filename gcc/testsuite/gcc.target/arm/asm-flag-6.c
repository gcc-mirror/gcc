/* Executable testcase for 'output flags.'  */
/* { dg-do run } */
/* { dg-skip-if "" { arm_thumb1 } } */

int test_bits (long nzcv)
{
  long n, z, c, v;

  __asm__ ("msr APSR_nzcvq, %[in]"
	   : "=@ccmi"(n), "=@cceq"(z), "=@cccs"(c), "=@ccvs"(v)
	   : [in] "r"(nzcv << 28));

  return n * 8 + z * 4 + c * 2 + v == nzcv;
}
	
int test_cmps (long x, long y)
{
  long gt, lt, ge, le;

  __asm__ ("cmp %[x], %[y]"
	   : "=@ccgt"(gt), "=@cclt"(lt), "=@ccge"(ge), "=@ccle"(le)
	   : [x] "r"(x), [y] "r"(y));

  return (gt == (x > y)
	  && lt == (x < y)
	  && ge == (x >= y)
	  && le == (x <= y));
}

int test_cmpu (unsigned long x, unsigned long y)
{
  long gt, lt, ge, le;

  __asm__ ("cmp %[x], %[y]"
	   : "=@cchi"(gt), "=@cclo"(lt), "=@cchs"(ge), "=@ccls"(le)
	   : [x] "r"(x), [y] "r"(y));

  return (gt == (x > y)
	  && lt == (x < y)
	  && ge == (x >= y)
	  && le == (x <= y));
}

int main ()
{
  long i, j;

  for (i = 0; i < 16; ++i)
    if (!test_bits (i))
      __builtin_abort ();

  for (i = -1; i <= 1; ++i)
    for (j = -1; j <= 1; ++j)
      if (!test_cmps (i, j))
        __builtin_abort ();

  for (i = 0; i <= 2; ++i)
    for (j = 0; j <= 2; ++j)
      if (!test_cmpu (i, j))
        __builtin_abort ();

  return 0;
}
