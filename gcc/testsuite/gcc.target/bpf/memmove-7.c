/* Check that the backward memmove loop moves the correct bytes for increments
   larger than 1.  PR target/128440.  */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal" } */

short sbuf[10];

void
g (void) {
  __builtin_memmove (&sbuf[1], &sbuf[0], 5);
}

/* { dg-final { scan-assembler-times {ldxb[^\r\n]+\[%r[0-9]\+4\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxh[^\r\n]+\[%r[0-9]\+2\]} 2 } } */
/* { dg-final { scan-assembler-times {ldxh[^\r\n]+\[%r[0-9]\+0\]} 2 } } */

/* No other offsets should be used.  */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+[1356-9]+\]} } } */
/* { dg-final { scan-assembler-not {\[%r[0-9]\-} } } */
