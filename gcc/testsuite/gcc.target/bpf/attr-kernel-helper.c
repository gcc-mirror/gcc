/* Basic test for kernel_helper attribute BTF information.  */

/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -masm=normal" } */

extern int foo_helper(int) __attribute((kernel_helper(42)));
extern int foo_nohelper(int);

int bar (int arg)
{
  return foo_helper (arg) + foo_nohelper (arg);
}

/* { dg-final { scan-assembler-times "BTF_KIND_FUNC 'foo_nohelper'" 1 } } */
/* { dg-final { scan-assembler-times "BTF_KIND_FUNC 'foo_helper'" 0 } } */
