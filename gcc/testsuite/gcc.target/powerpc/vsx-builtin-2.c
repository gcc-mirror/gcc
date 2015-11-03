/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler "xvaddsp" } } */
/* { dg-final { scan-assembler "xvsubsp" } } */
/* { dg-final { scan-assembler "xvmulsp" } } */
/* { dg-final { scan-assembler "vmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */
/* { dg-final { scan-assembler "xvnmadd" } } */
/* { dg-final { scan-assembler "vnmsub" } } */
/* { dg-final { scan-assembler "xvdivsp" } } */
/* { dg-final { scan-assembler "xvmaxsp" } } */
/* { dg-final { scan-assembler "xvminsp" } } */
/* { dg-final { scan-assembler "xvsqrtsp" } } */
/* { dg-final { scan-assembler "xvabssp" } } */
/* { dg-final { scan-assembler "xvnabssp" } } */
/* { dg-final { scan-assembler "xvresp" } } */
/* { dg-final { scan-assembler "xvrsqrtesp" } } */

void use_builtins (__vector float *p, __vector float *q, __vector float *r, __vector float *s)
{
  p[0] = __builtin_vsx_xvaddsp (q[0], r[0]);
  p[1] = __builtin_vsx_xvsubsp (q[1], r[1]);
  p[2] = __builtin_vsx_xvmulsp (q[2], r[2]);
  p[3] = __builtin_vsx_xvdivsp (q[3], r[3]);
  p[4] = __builtin_vsx_xvmaxsp (q[4], r[4]);
  p[5] = __builtin_vsx_xvminsp (q[5], r[5]);
  p[6] = __builtin_vsx_xvabssp (q[6]);
  p[7] = __builtin_vsx_xvnabssp (q[7]);
  p[8] = __builtin_vsx_xvsqrtsp (q[8]);
  p[9] = __builtin_vsx_xvmaddsp (q[9], r[9], s[9]);
  p[10] = __builtin_vsx_xvmsubsp (q[10], r[10], s[10]);
  p[11] = __builtin_vsx_xvnmaddsp (q[11], r[11], s[11]);
  p[12] = __builtin_vsx_xvnmsubsp (q[12], r[12], s[12]);
  p[13] = __builtin_vsx_xvresp (q[13]);
  p[14] = __builtin_vsx_xvrsqrtesp (q[14]);
}
