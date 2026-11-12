/* Check that the backward memmove loop moves the correct bytes for increments
   larger than 1.  PR target/128440.  */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal" } */

long  lbuf[4];

void
g (void) {
  __builtin_memmove (&lbuf[1], &lbuf[0], 13);
}

/* { dg-final { scan-assembler-times {ldxb[^\r\n]+\[%r[0-9]\+12\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxw[^\r\n]+\[%r[0-9]\+8\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxdw[^\r\n]+\[%r[0-9]\+0\]} 2 } } */
/* { dg-final { scan-assembler-times {stxb[^\r\n]+\[%r[0-9]\+12\]} 2 } } */
/* { dg-final { scan-assembler-times {stxw[^\r\n]+\[%r[0-9]\+8\]} 2 } } */
/* { dg-final { scan-assembler-times {stxdw[^\r\n]+\[%r[0-9]\+0\]} 2 } } */

/* No other offsets should be used.  */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+[2-79]+\]} } } */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+1\]} } } */
/* { dg-final { scan-assembler-not {\[%r[0-9]\-} } } */
