/* PR target/96808 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we do not ICE on the tests below.  */

void
old_ok (__vector_quad *dst, vector unsigned char vc)
{
  __vector_quad vq;
  __builtin_mma_xxsetaccz(&vq);
  __builtin_mma_xvf32gerpp(&vq, vc, vc);
  *dst = vq;
}

void
test0 (__vector_quad *dst, vector unsigned char vc)
{
  __vector_quad vq[2];
  __builtin_mma_xxsetaccz(&vq[1]);
  __builtin_mma_xvf32gerpp(&vq[1], vc, vc);
  *dst = vq[1];
}

void
test1 (__vector_quad *dst, vector unsigned char vc)
{
  __vector_quad vq[2][2];
  __builtin_mma_xxsetaccz(&vq[1][1]);
  __builtin_mma_xvf32gerpp(&vq[1][1], vc, vc);
  *dst = vq[1][1];
}

void
test2 (__vector_quad *dst, vector unsigned char vc)
{
  struct {
    __vector_quad dummy;
    __vector_quad acc;
  } vq;
  __builtin_mma_xxsetaccz(&vq.acc);
  __builtin_mma_xvf32gerpp(&vq.acc, vc, vc);
  *dst = vq.acc;
}

void
test3 (__vector_quad *dst, vector unsigned char vc)
{
  __builtin_mma_xxsetaccz(&dst[1]);
  __builtin_mma_xvf32gerpp(&dst[1], vc, vc);
}

void
test4 (__vector_quad *dst[], vector unsigned char vc)
{
  __builtin_mma_xxsetaccz(&dst[1][2]);
  __builtin_mma_xvf32gerpp(&dst[1][2], vc, vc);
}
