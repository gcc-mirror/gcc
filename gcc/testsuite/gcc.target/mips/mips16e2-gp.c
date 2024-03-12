/* { dg-options "-mno-abicalls -mgpopt -G8 -mabi=32 -mips16 -mmips16e2" } */
/* { dg-skip-if "per-function expected output" { *-*-* } { "-flto" } { "" } } */
 
/* Generate GP-relative ADDIU.  */

/* { dg-final { scan-assembler "test01:.*\taddiu\t\\\$2,\\\$28.*test01\n" } } */
int arr[2];
   
int *
test01 (void)
{
  return (&arr[1]);
}

/* Test LB[GP].  */

/* { dg-final { scan-assembler "test02:.*\tlb\t.*\\\$28.*test02\n" } } */
signed char c02;

signed char
test02 (void) 
{
  return c02;
}

/* Test LBU[GP].  */

/* { dg-final { scan-assembler "test03:.*\tlbu\t.*\\\$28.*test03\n" } } */
unsigned char uc03;

unsigned char
test03 (void)
{
  return uc03;
}

/* Test LH[GP].  */

/* { dg-final { scan-assembler "test04:.*\tlh\t.*\\\$28.*test04\n" } } */
short s04;

short
test04 (void)
{
  return s04;
}

/* Test LHU[GP].  */

/* { dg-final { scan-assembler "test05:.*\tlhu\t.*\\\$28.*test05\n" } } */
unsigned short s05;

unsigned short
test05 (void)
{
  return s05;
}

/* Test LW[GP].  */

/* { dg-final { scan-assembler "test06:.*\tlw\t.*\\\$28.*test06\n" } } */
int i06;

int
test06 (void)
{
  return i06;
}

/* Test SB[GP].  */

/* { dg-final { scan-assembler "test07:.*\tsb\t.*\\\$28.*test07\n" } } */
char c07;

void
test07 (char x)
{
  c07 = x;
}

/* Test SH[GP].  */

/* { dg-final { scan-assembler "test08:.*\tsh\t.*\\\$28.*test08\n" } } */
short s08;

void
test08 (short x)
{
  s08 = x;
}

/* Test SW[GP].  */

/* { dg-final { scan-assembler "test09:.*\tsw\t.*\\\$28.*test09\n" } } */
int i09;

void
test09 (int x)
{
  i09 = x;
}
