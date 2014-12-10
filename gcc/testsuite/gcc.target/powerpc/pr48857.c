/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -mabi=altivec" } */
/* { dg-final { scan-assembler-times "lxvd2x" 1 } } */
/* { dg-final { scan-assembler-times "stxvd2x" 1 } } */
/* { dg-final { scan-assembler-not "ld" } } */
/* { dg-final { scan-assembler-not "lwz" } } */
/* { dg-final { scan-assembler-not "stw" } } */
/* { dg-final { scan-assembler-not "addi" } } */

typedef vector long long v2di_type;

v2di_type
return_v2di (v2di_type *ptr)
{
  return *ptr;		/* should generate lxvd2x 34,0,3.  */
}

void
pass_v2di (v2di_type arg, v2di_type *ptr)
{
  *ptr = arg;		/* should generate stxvd2x 34,0,{3,5}.  */
}

