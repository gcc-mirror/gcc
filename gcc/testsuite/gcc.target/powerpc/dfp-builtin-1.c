/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target hard_dfp } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "ddedpd " 4    } } */
/* { dg-final { scan-assembler-times "denbcd " 2    } } */
/* { dg-final { scan-assembler-times "dxex "   1    } } */
/* { dg-final { scan-assembler-times "diex "   1    } } */
/* { dg-final { scan-assembler-times "dscli "  2    } } */
/* { dg-final { scan-assembler-times "dscri "  2    } } */
/* { dg-final { scan-assembler-times "std "    1    { target lp64 } } } */
/* { dg-final { scan-assembler-times "ld "     1    { target lp64 } } } */
/* 32-bit needs a stack frame, and needs two GPR mem insns per _Decimal64.  */
/* { dg-final { scan-assembler-times "stwu "   2    { target ilp32 } } } */
/* { dg-final { scan-assembler-times "stw "    2    { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lwz "    2    { target ilp32 } } } */
/* { dg-final { scan-assembler-times "stfd "   1    } } */
/* { dg-final { scan-assembler-times "lfd "    1    } } */
/* { dg-final { scan-assembler-not   "bl __builtin" } } */
/* { dg-final { scan-assembler-not   "dctqpq"       } } */
/* { dg-final { scan-assembler-not   "drdpq"        } } */

_Decimal64
do_dedpd_0 (_Decimal64 a)
{
  return __builtin_ddedpd (0, a);
}

_Decimal64
do_dedpd_1 (_Decimal64 a)
{
  return __builtin_ddedpd (1, a);
}

_Decimal64
do_dedpd_2 (_Decimal64 a)
{
  return __builtin_ddedpd (2, a);
}

_Decimal64
do_dedpd_3 (_Decimal64 a)
{
  return __builtin_ddedpd (3, a);
}

_Decimal64
do_enbcd_0 (_Decimal64 a)
{
  return __builtin_denbcd (0, a);
}

_Decimal64
do_enbcd_1 (_Decimal64 a)
{
  return __builtin_denbcd (1, a);
}

long long
do_xex (_Decimal64 a)
{
  return __builtin_dxex (a);
}

_Decimal64
do_iex (long long a, _Decimal64 b)
{
  return __builtin_diex (a, b);
}

_Decimal64
do_scli_1 (_Decimal64 a)
{
  return __builtin_dscli (a, 1);
}

_Decimal64
do_scli_10 (_Decimal64 a)
{
  return __builtin_dscli (a, 10);
}

_Decimal64
do_scri_1 (_Decimal64 a)
{
  return __builtin_dscri (a, 1);
}

_Decimal64
do_scri_10 (_Decimal64 a)
{
  return __builtin_dscri (a, 10);
}
