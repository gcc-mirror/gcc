/* { dg-options "-mno-abicalls -mgpopt -G8 -mabi=32 -mips16 -mmips16e2 -mbranch-cost=2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* Test MOVN.  */

/* { dg-final { scan-assembler-times "test01:.*\tmovn\t.*test01\n" 1 } } */
int
test01 (int a, int b, int c)
{
  return (a==0) ? b : c;
}

/* { dg-final { scan-assembler-times "test02:.*\tmovn\t\\\$.,\\\$0.*test02\n" 1 } } */
int
test02 (int a, int b, int c)
{
  return (a==0) ? b : 0;
}

/* Test MOVZ.  */

/* { dg-final { scan-assembler-times "test03:.*\tmovz\t.*test03\n" 1 } } */
int
test03 (int a, int b, int c)
{
  return a ? b : c;
}

/* { dg-final { scan-assembler-times "test04:.*\tmovz\t\\\$.,\\\$0.*test04\n" 1 } } */
int
test04 (int a, int b, int c)
{
  return a ? b : 0;
}

/* Test MOVTN.  */

/* { dg-final { scan-assembler-times "test05:.*\tmovtn\t.*test05\n" 1 } } */
int
test05 (int a, int b, int c, int d)
{
  return a >= b ? c : d;
}

/* { dg-final { scan-assembler-times "test06:.*\tmovtn\t\\\$2,\\\$0.*test06\n" 1 } } */
int
test06 (int a, int b, int c, int d)
{
  return a >= b ? c : 0;
}

/* Test MOVTZ.  */

/* { dg-final { scan-assembler-times "test07:.*\tmovtz\t.*test07\n" 1 } } */
int
test07 (int a, int b, int c, int d)
{
  return a < b ? c : d;
}

/* { dg-final { scan-assembler-times "test08:.*\tmovtz\t\\\$.,\\\$0.*test08\n" 1 } } */
int
test08 (int a, int b, int c, int d)
{
  return a < b ? c : 0;
}


