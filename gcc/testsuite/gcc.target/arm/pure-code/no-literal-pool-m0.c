/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m0_ok } */
/* { dg-options "-mpure-code" } */
/* { dg-add-options arm_cpu_cortex_m0 }*/

/* Does not use thumb1_gen_const_int.  */
int
test_0 ()
{
  return 0;
}

/* Does not use thumb1_gen_const_int.  */
int
test_128 ()
{
  return 128;
}

/* Does not use thumb1_gen_const_int.  */
int
test_264 ()
{
  return 264;
}

/* Does not use thumb1_gen_const_int.  */
int
test_510 ()
{
  return 510;
}

/* Does not use thumb1_gen_const_int.  */
int
test_512 ()
{
  return 512;
}

/* Does not use thumb1_gen_const_int.  */
int
test_764 ()
{
  return 764;
}

/* Does not use thumb1_gen_const_int.  */
int
test_65536 ()
{
  return 65536;
}

int
test_0x123456 ()
{
  return 0x123456;
}

int
test_0x1123456 ()
{
  return 0x1123456;
}

int
test_0x1000010 ()
{
  return 0x1000010;
}

int
test_0x1000011 ()
{
  return 0x1000011;
}

int
test_m8192 ()
{
  return -8192;
}

/* { dg-final { scan-assembler-not "\tldr\tr\[0-9\]+, \\.L\[0-9\]+" } } */
