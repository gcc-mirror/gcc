/* Check that the backward memmove loop moves the correct bytes for increments
   larger than 1.  PR target/128440.  */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal" } */

int ibuf[4];

void
g (void) {
  __builtin_memmove (&ibuf[1], &ibuf[0], 11);
}

/* { dg-final { scan-assembler-times {ldxb[^\r\n]+\[%r[0-9]\+10\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxh[^\r\n]+\[%r[0-9]\+8\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxw[^\r\n]+\[%r[0-9]\+4\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxw[^\r\n]+\[%r[0-9]\+0\]} 2 } } */

/* No other offsets should be used.  */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+[235679]+\]} } } */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+1\]} } } */
/* { dg-final { scan-assembler-not {\[%r[0-9]\-} } } */
