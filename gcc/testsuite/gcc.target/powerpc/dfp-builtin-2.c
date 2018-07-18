/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target hard_dfp } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "ddedpdq " 4    } } */
/* { dg-final { scan-assembler-times "denbcdq " 2    } } */
/* { dg-final { scan-assembler-times "dxexq "   1    } } */
/* { dg-final { scan-assembler-times "diexq "   1    } } */
/* { dg-final { scan-assembler-times "dscliq "  2    } } */
/* { dg-final { scan-assembler-times "dscriq "  2    } } */
/* { dg-final { scan-assembler-not    "bl __builtin" } } */
/* { dg-final { scan-assembler-not   "dctqpq"        } } */
/* { dg-final { scan-assembler-not   "drdpq"         } } */
/* { dg-final { scan-assembler-not   "stfd"          } } */
/* { dg-final { scan-assembler-not   "lfd"           } } */

_Decimal128
do_dedpdq_0 (_Decimal128 a)
{
  return __builtin_ddedpdq (0, a);
}

_Decimal128
do_dedpdq_1 (_Decimal128 a)
{
  return __builtin_ddedpdq (1, a);
}

_Decimal128
do_dedpdq_2 (_Decimal128 a)
{
  return __builtin_ddedpdq (2, a);
}

_Decimal128
do_dedpdq_3 (_Decimal128 a)
{
  return __builtin_ddedpdq (3, a);
}

_Decimal128
do_enbcdq_0 (_Decimal128 a)
{
  return __builtin_denbcdq (0, a);
}

_Decimal128
do_enbcdq_1 (_Decimal128 a)
{
  return __builtin_denbcdq (1, a);
}

_Decimal128
do_xexq (_Decimal128 a)
{
  return __builtin_dxexq (a);
}

_Decimal128
do_iexq (_Decimal128 a, _Decimal128 b)
{
  return __builtin_diexq (a, b);
}

_Decimal128
do_scliq_1 (_Decimal128 a)
{
  return __builtin_dscliq (a, 1);
}

_Decimal128
do_scliq_10 (_Decimal128 a)
{
  return __builtin_dscliq (a, 10);
}

_Decimal128
do_scriq_1 (_Decimal128 a)
{
  return __builtin_dscriq (a, 1);
}

_Decimal128
do_scriq_10 (_Decimal128 a)
{
  return __builtin_dscriq (a, 10);
}
