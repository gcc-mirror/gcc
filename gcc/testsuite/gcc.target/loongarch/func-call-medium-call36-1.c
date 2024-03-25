/* { dg-do compile } */
/* { dg-require-effective-target loongarch_call36_support } */
/* { dg-options "-mcmodel=medium -mexplicit-relocs -fdump-rtl-final -O2" } */
/* { dg-final { scan-assembler "test:.*pcaddu18i\t\\\$r1,%call36\\(func\\)" } } */
/* { dg-final { scan-assembler "test_value:.*pcaddu18i\t\\\$r1,%call36\\(func_value\\)" } } */

extern void func (void);
int
test (void)
{
  func ();
}


extern int func_value (void);
float
test_value (void)
{
  func_value ();
}

