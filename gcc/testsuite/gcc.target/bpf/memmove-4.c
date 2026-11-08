/* Check that the memmove loop offsets are sane when using increments
   larger than 1.  PR target/128440.  */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal" } */

int buf[4];

void
f (void) {
  __builtin_memmove (&buf[1], &buf[0], 12);
}

/* { dg-final { scan-assembler-times {\[%r[0-9]\+4\]} 4 } } */
/* { dg-final { scan-assembler-times {\[%r[0-9]\+8\]} 4 } } */
/* { dg-final { scan-assembler-times {\[%r[0-9]\+0\]} 4 } } */

/* No other offsets should be used.  */
/* { dg-final { scan-assembler-not {\[%r[0-9]\+[123579]+\]} } } */
