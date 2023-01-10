/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* Verify that two instructions are successfully used to build constants.
   One insn is li or lis, another is rotate: rldicl or rldicr.  */

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

long long NOIPA
li_rldicl_7 (void)
{
  return 0x3ffffffa1LL;
}

long long NOIPA
li_rldicl_8 (void)
{
  return 0xff8531ffffffffLL;
}

long long NOIPA
lis_rldicl_9 (void)
{
  return 0x00ff85310000ffffLL;
}

long long NOIPA
li_rldicr_10 (void)
{
  return 0xffff8531fff00000LL;
}

long long NOIPA
li_rldicr_11 (void)
{
  return 0x21fffffffff00000LL;
}

long long NOIPA
lis_rldicr_12 (void)
{
  return 0x5310000ffffffff0LL;
}

struct fun arr[] = {
  {li_rotldi_1, 0x7531000000000LL},
  {li_rotldi_2, 0x2100000000000064LL},
  {li_rotldi_3, 0xffff8531ffffffffLL},
  {li_rotldi_4, 0x21ffffffffffff94LL},
  {lis_rotldi_5, 0xffff85310000ffffLL},
  {lis_rotldi_6, 0x5310000ffffffff8LL},
  {li_rldicl_7, 0x3ffffffa1LL},
  {li_rldicl_8, 0xff8531ffffffffLL},
  {lis_rldicl_9, 0x00ff85310000ffffLL},
  {li_rldicr_10, 0xffff8531fff00000LL},
  {li_rldicr_11, 0x21fffffffff00000LL},
  {lis_rldicr_12, 0x5310000ffffffff0LL},
};

/* { dg-final { scan-assembler-times {\mrotldi\M} 6 } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 3 } } */
/* { dg-final { scan-assembler-times {\mrldicr\M} 3 } } */

int
main ()
{
  for (int i = 0; i < sizeof (arr) / sizeof (arr[0]); i++)
    if ((*arr[i].f) () != arr[i].val)
      __builtin_abort ();

  return 0;
}
