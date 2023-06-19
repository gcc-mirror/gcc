/* { dg-options "-mno-abicalls -mgpopt -G8 -mabi=32 -mips16 -mmips16e2" } */
/* { dg-skip-if "per-function expected output" { *-*-* } { "-flto" } { "" } } */
 
/* ANDI is a two operand instruction.  Hence, it won't be generated if src and
 *    dest are in different registers.  */
   
/* { dg-final { scan-assembler "test01:.*\tandi\t.*test01\n" } } */
unsigned int
test01 (unsigned int a)
{
  return ((a + 0x2) & 0x3ff);
}

/* Test EXT */

/* { dg-final { scan-assembler "test02:.*\text\t.*test02\n" } } */
struct
{
  unsigned int a:9;
  unsigned int d:31;
  unsigned int e:9;
  unsigned int f:10;
} t02;

unsigned int
test02 (void)
{
  return t02.f;
}

/* Use EXT when ANDing with low-order bitmasks.  */

/* { dg-final { scan-assembler "test03:.*\text\t.*test03\n" } } */
/* { dg-final { scan-assembler-not "test03.*\tandi?\t.*test03\n" } } */
unsigned int
test03 (unsigned int x)
{
  return (x & 0x1fffffff);
}

/* Test INS */

/* { dg-final { scan-assembler "test04:.*\tins\t.*test04\n" } } */
struct
{
  unsigned int i : 9;
  unsigned int j : 15;
  unsigned int k : 4;
} s04;

void
test04 (void)
{
  s04.j = 1;
}

/* Use INS with hardcoded $0.  */

/* { dg-final { scan-assembler "test05:.*\tins\t\\\$.*,\\\$0.*test05\n" } } */
struct
{
  unsigned int i : 8;
  unsigned int j : 9;
  unsigned int k : 10;
} __attribute__ ((packed)) s05 __attribute__((aligned(1)));

void
test05 (void)
{
  s05.k = 0;
}

/* Use INS when ANDing to clear only one consecutive chunk of bits.  */

/* { dg-final { scan-assembler "test06:.*\tins\t\\\$.*,\\\$0,11,5.*test06\n" } } */
/* { dg-final { scan-assembler-not "test06:.*\tandi?\t.*test06\n" } } */
unsigned int
test06 (unsigned int x)
{
  return (x & 0xffff07ff);
}

/* ORI is a two operand instruction.  Hence, it won't be generated if src and
   dest are in different registers.  */

/* { dg-final { scan-assembler "test07:.*\tori\t.*test07\n" } } */
unsigned int
test07 (unsigned int a)
{
  return (a + 0x2) | 0x7f0;
}

/* XORI is a two operand instruction.  Hence, it won't be generated if src and
   dest are in different registers.  */

/* { dg-final { scan-assembler "test08:.*\txori\t.*test08\n" } } */
unsigned int
test08 (unsigned int a)
{
  return ((a + 0x2) ^ 0x3f0);
}

