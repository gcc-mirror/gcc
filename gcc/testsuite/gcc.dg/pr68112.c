/* { dg-do compile } */
/* { dg-options "-O2 -Waggressive-loop-optimizations" } */

int *a;

void
foo ()
{
  for (int i = 0; i < 65536; i++)
    *a = i << 24;
}
