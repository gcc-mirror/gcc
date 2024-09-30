/* { dg-do compile } */
/* { dg-options "-masm=normal" } */

long delta;
long *val;

void
foo ()
{
  __sync_fetch_and_add(val, delta);
  __sync_fetch_and_add((int *)val, (int)delta);
}

/* { dg-final { scan-assembler "afadd\t.*" } } */
/* { dg-final { scan-assembler "afadd32\t.*" } } */
