/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

void Test(long long Val, int Amt)
{
  __builtin_printf("  lshr: 0x%llx \t\t shl: 0x%llx\n", Val >> Amt, Val << Amt);  /* count(1) */
  __builtin_printf("  lshr: 0x%llx\t\tshl: 0x%llx\n",  /* count(1) */
    Val >> Amt, Val << Amt);
  __builtin_printf("  lshr: 0x%llx \t\t shl: 0x%llx\n",  /* count(1) */
    (unsigned long long)Val >> Amt, Val << Amt);
}

int main()
{
  Test(10, 4);

  return 0;
}


/* { dg-final { run-gcov gcov-pr85338.c } } */
