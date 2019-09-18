/* { dg-do compile } */

long delta;
long *val;

void
foo ()
{
  __sync_fetch_and_add(val, delta);
  __sync_fetch_and_add((int *)val, (int)delta);
}

/* { dg-final { scan-assembler "xadddw\t.*" } } */
/* { dg-final { scan-assembler "xaddw\t.*" } } */
