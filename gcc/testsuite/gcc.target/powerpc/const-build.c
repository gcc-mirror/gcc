/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* Verify that two instructions are successfully used to build constants.
   One insn is li or lis, another is rotate: rldicl.  */

#define NOIPA __attribute__ ((noipa))

struct fun
{
  long long (*f) (void);
  long long val;
};

long long NOIPA
li_rotldi_1 (void)
{
  return 0x7531000000000LL;
}

long long NOIPA
li_rotldi_2 (void)
{
  return 0x2100000000000064LL;
}

long long NOIPA
li_rotldi_3 (void)
{
  return 0xffff8531ffffffffLL;
}

long long NOIPA
li_rotldi_4 (void)
{
  return 0x21ffffffffffff94LL;
}

long long NOIPA
lis_rotldi_5 (void)
{
  return 0xffff85310000ffffLL;
}

long long NOIPA
lis_rotldi_6 (void)
{
  return 0x5310000ffffffff8LL;
}

struct fun arr[] = {
  {li_rotldi_1, 0x7531000000000LL},
  {li_rotldi_2, 0x2100000000000064LL},
  {li_rotldi_3, 0xffff8531ffffffffLL},
  {li_rotldi_4, 0x21ffffffffffff94LL},
  {lis_rotldi_5, 0xffff85310000ffffLL},
  {lis_rotldi_6, 0x5310000ffffffff8LL},
};

/* { dg-final { scan-assembler-times {\mrotldi\M} 6 } } */

int
main ()
{
  for (int i = 0; i < sizeof (arr) / sizeof (arr[0]); i++)
    if ((*arr[i].f) () != arr[i].val)
      __builtin_abort ();

  return 0;
}
