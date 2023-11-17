/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadint -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadint -mabi=lp64d" { target { rv64 } } } */

extern void f (void);

__attribute__ ((interrupt))
void func_default (void)
{
  f ();
}

__attribute__ ((interrupt ("machine")))
void func_machine (void)
{
  f ();
}

/* { dg-final { scan-assembler-times {\mth\.ipush\M} 2 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times {\mth\.ipop\M} 2 { target { rv32 } } } } */


__attribute__ ((interrupt ("user")))
void func_usr (void)
{
  f ();
}

__attribute__ ((interrupt ("supervisor")))
void func_supervisor (void)
{
  f ();
}

/* { dg-final { scan-assembler-not {\mth\.ipush\M} { target { rv64 } } } } */
/* { dg-final { scan-assembler-not {\mth\.ipop\M} { target { rv64 } } } } */
