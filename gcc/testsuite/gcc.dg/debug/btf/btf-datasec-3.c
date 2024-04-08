/* PR debug/112849
   Test that we do not incorrectly create BTF_KIND_DATASEC entries for
   extern decls with no known section.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

extern int VERSION __attribute__((section (".version")));

extern int ext1;
extern int ext2;

int var1 __attribute__((section (".sec_a")));
int var2 __attribute__((section (".sec_b"))) = 2;

int
foo (void)
{
  ext2 = VERSION;
  return ext1 + var1 + var2;
}

/* There should be 3 DATASEC entries total.  Of the extern decls, only VERSION
   has a known section; entries are not created for the other two.  */
/* { dg-final { scan-assembler-times "bts_type" 3 } } */
/* { dg-final { scan-assembler-times "bts_type: \\(BTF_KIND_VAR 'VERSION'\\)" 1 } } */
/* { dg-final { scan-assembler-not "bts_type: \\(BTF_KIND_VAR 'ext1'\\)" } } */
/* { dg-final { scan-assembler-not "bts_type: \\(BTF_KIND_VAR 'ext2'\\)" } } */
