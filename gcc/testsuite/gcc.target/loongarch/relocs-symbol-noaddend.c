/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -mexplicit-relocs -fno-pic -O2 -mcmodel=normal" } */
/* { dg-final { scan-assembler "pcalau12i.*%pc_hi20\\(\.LANCHOR0\\)\n" } } */
/* { dg-final { scan-assembler "addi\.d.*%pc_lo12\\(\.LANCHOR0\\)\n" } } */
/* { dg-final { scan-assembler "ldptr.d\t\\\$r4,.*,0\n" } } */
/* { dg-final { scan-assembler "ld.d\t\\\$r5,.*,8\n" } } */
/* { dg-final { scan-assembler-not  "\.LANCHOR0+8" } } */


struct S
{
  char *a;
  unsigned short int b;
};

struct S s1;

void test(struct S);
void test1(void)
{
  test(s1);
}

