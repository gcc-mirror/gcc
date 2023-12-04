/* PR debug/112849
   Test that we do not incorrectly create BTF_KIND_DATASEC entries for
   extern decls with no known section.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

extern int VERSION __attribute__((section (".version")));

extern int test_bss1;
extern int test_data1;

int test_bss2;
int test_data2 = 2;

int
foo (void)
{
  test_bss2 = VERSION;
  return test_bss1 + test_data1 + test_data2;
}

/* There should be 3 DATASEC entries total.  Of the extern decls, only VERSION
   has a known section; entries are not created for the other two.  */
/* { dg-final { scan-assembler-times "bts_type" 3 } } */
/* { dg-final { scan-assembler-times "bts_type: \\(BTF_KIND_VAR 'test_data2'\\)" 1 } } */
/* { dg-final { scan-assembler-times "bts_type: \\(BTF_KIND_VAR 'test_bss2'\\)" 1 } } */
/* { dg-final { scan-assembler-times "bts_type: \\(BTF_KIND_VAR 'VERSION'\\)" 1 } } */
