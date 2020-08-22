/* { dg-do compile } */
/* { dg-options "-g -O2 -gdwarf -dA" } */

extern void foo (int);
extern void unusedbar (int);

int main()
{
  foo (1);
}

/* We want subprogram DIEs for both foo and main and a DIE for
   the formal parameter of foo.  We do not want a DIE for
   unusedbar.  */
/* { dg-final { scan-assembler-times "DW_TAG_subprogram" 4 } } */
/* { dg-final { scan-assembler-times "DW_TAG_formal_parameter" 2 } } */
/* { dg-final { scan-assembler-not "unusedbar" } } */
