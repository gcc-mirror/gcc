/* { dg-do compile } */
/* { dg-options "-mcpu=8548 -mspe -mabi=spe -O2" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } { "*" } { "" } } */

/* In PR60735, the type _Decimal64 generated an insn not found message.  */

void
pr60735 (_Decimal64 *p, _Decimal64 *q)
{
  *p = *q;
}
