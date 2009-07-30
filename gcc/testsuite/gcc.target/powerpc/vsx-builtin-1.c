/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler "xvadddp" } } */
/* { dg-final { scan-assembler "xvsubdp" } } */
/* { dg-final { scan-assembler "xvmuldp" } } */
/* { dg-final { scan-assembler "xvmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */
/* { dg-final { scan-assembler "xvnmadd" } } */
/* { dg-final { scan-assembler "xvnmsub" } } */
/* { dg-final { scan-assembler "xvdivdp" } } */
/* { dg-final { scan-assembler "xvmaxdp" } } */
/* { dg-final { scan-assembler "xvmindp" } } */
/* { dg-final { scan-assembler "xvsqrtdp" } } */
/* { dg-final { scan-assembler "xvrsqrtedp" } } */
/* { dg-final { scan-assembler "xvabsdp" } } */
/* { dg-final { scan-assembler "xvnabsdp" } } */
/* { dg-final { scan-assembler "xvredp" } } */

void use_builtins (__vector double *p, __vector double *q, __vector double *r, __vector double *s)
{
  p[0] = __builtin_vsx_xvadddp (q[0], r[0]);
  p[1] = __builtin_vsx_xvsubdp (q[1], r[1]);
  p[2] = __builtin_vsx_xvmuldp (q[2], r[2]);
  p[3] = __builtin_vsx_xvdivdp (q[3], r[3]);
  p[4] = __builtin_vsx_xvmaxdp (q[4], r[4]);
  p[5] = __builtin_vsx_xvmindp (q[5], r[5]);
  p[6] = __builtin_vsx_xvabsdp (q[6]);
  p[7] = __builtin_vsx_xvnabsdp (q[7]);
  p[8] = __builtin_vsx_xvsqrtdp (q[8]);
  p[9] = __builtin_vsx_xvmadddp (q[9], r[9], s[9]);
  p[10] = __builtin_vsx_xvmsubdp (q[10], r[10], s[10]);
  p[11] = __builtin_vsx_xvnmadddp (q[11], r[11], s[11]);
  p[12] = __builtin_vsx_xvnmsubdp (q[12], r[12], s[12]);
  p[13] = __builtin_vsx_xvredp (q[13]);
  p[14] = __builtin_vsx_xvrsqrtedp (q[14]);
}
