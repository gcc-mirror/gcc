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

/* Test LUI.  */

/* { dg-final { scan-assembler "test09:.*\tlui\t.*test09\n" } } */
int
test09 (void)
{
  return 0x44440000;
}

/* Test LUI relocation sequence chang.  */

/* { dg-final { scan-assembler "test10:.*\tlui\t.*test10\n" } } */
int *a10;

int
test10 (int i)
{
  a10 = &i;
  *a10 = 0x44440000;
  return i;
}

/* Test 32bit unaligned load.  */

/* { dg-final { scan-assembler "test11:.*\tlwl\t.*test11\n" } } */
/* { dg-final { scan-assembler "test11:.*\tlwr\t.*test11\n" } } */
struct node11
{
  char c;
  int i;
} __attribute__ ((packed)) obj11 __attribute__((aligned(1)));

int
test11 (void)
{
  return obj11.i;
}

/* Test 32bit unaligned load.  */

/* { dg-final { scan-assembler "test12:.*\tlwl\t.*test12\n" } } */
/* { dg-final { scan-assembler "test12:.*\tlwr\t.*test12\n" } } */
struct node12
{
  unsigned int i : 8;
  unsigned int j : 32;
} __attribute__ ((packed)) obj12 __attribute__((aligned(16)));

int
test12 (void)
{
  return obj12.j;
}

/* Test 32bit unaligned store with non-zero constant */

/* { dg-final { scan-assembler "test13:.*\tswl\t.*test13\n" } } */
/* { dg-final { scan-assembler "test13:.*\tswr\t.*test13\n" } } */
struct node13
{
  char c;
  int i;
} __attribute__ ((packed)) obj13 __attribute__((aligned(1)));

void
test13 (void)
{
  obj13.i = 1234;
}

/* Test 32bit unaligned store with zero constant.  */

/* { dg-final { scan-assembler "test14:.*\tswl\t.*test14\n" } } */
/* { dg-final { scan-assembler "test14:.*\tswr\t.*test14\n" } } */
/* { dg-final { scan-assembler-not "test14:.*\tswl\t\\\$0,.*test14\n" } } */
/* { dg-final { scan-assembler-not "test14:.*\tswr\t\\\$0,.*test14\n" } } */
struct node14
{
  char c;
  int i;
} __attribute__ ((packed)) obj14 __attribute__((aligned(1)));

void
test14 (void)
{
  obj14.i = 0;
}

/* Test 32bit unaligned store with non-constant.  */

/* { dg-final { scan-assembler "test15:.*\tswl\t.*test15\n" } } */
/* { dg-final { scan-assembler "test15:.*\tswr\t.*test15\n" } } */
struct node15
{
  char c;
  int i;
} __attribute__ ((packed)) obj15 __attribute__((aligned(1)));

int i15 = 1234;

void
test15 (void)
{
  obj15.i = i15;
}

/* Test 32bit unaligned store with non-constant */

/* { dg-final { scan-assembler "test16:.*\tswl\t.*test16\n" } } */
/* { dg-final { scan-assembler "test16:.*\tswr\t.*test16\n" } } */
struct node16
{
  char c;
  int i;
} __attribute__ ((packed)) obj16 __attribute__((aligned(1)));

void
test16 (int i)
{
  obj16.i = i;
}

/* Test 32bit unaligned store with non-constant.  */

/* { dg-final { scan-assembler "test17:.*\tswl\t.*test17\n" } } */
/* { dg-final { scan-assembler "test17:.*\tswr\t.*test17\n" } } */
struct node17
{
  unsigned int i : 8;
  unsigned int j : 32;
} __attribute__ ((packed)) obj17 __attribute__((aligned(16)));

void
test17 (int i)
{
  obj17.j = i;
}

