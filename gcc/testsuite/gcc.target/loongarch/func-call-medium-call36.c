/* { dg-do compile } */
/* { dg-require-effective-target loongarch_call36_support } */
/* { dg-options "-mcmodel=medium -mexplicit-relocs -fdump-rtl-final -O2" } */
/* { dg-final { scan-rtl-dump-times "\\(clobber \\(reg:DI 12 \\\$r12\\)\\)" 3 "final" } } */
/* { dg-final { scan-assembler "test:.*pcaddu18i\t\\\$r12,%call36\\(func\\)" } } */
/* { dg-final { scan-assembler "test_value:.*pcaddu18i\t\\\$r12,%call36\\(func_value\\)" } } */
/* { dg-final { scan-assembler "test_multi:.*pcaddu18i\t\\\$r12,%call36\\(func_multi\\)" } } */

extern void func (void);
void
test (void)
{
  func();
}


extern int func_value (void);
int
test_value (void)
{
  func_value ();
}

struct t {float a; float b;};

extern struct t func_multi (void);
struct t
test_multi (void)
{
  func_multi ();
}

