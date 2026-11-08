/* Check that the memmove loop offsets are sane when using increments
   larger than 1.  PR target/128440.  */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal" } */

long  lbuf[4];

void
g (void) {
  __builtin_memmove (&lbuf[1], &lbuf[0], 16);
}

/* { dg-final { scan-assembler-times {\[%r[0-9]\+8\]} 4 } } */
/* { dg-final { scan-assembler-times {\[%r[0-9]\+0\]} 4 } } */

/* No other offsets should be used.  */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+[1-79]+\]} } } */
