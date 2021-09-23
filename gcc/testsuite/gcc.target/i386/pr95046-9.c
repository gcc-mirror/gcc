/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -msse3" } */

float r[2], a[2], b[2];

void
test (void)
{
  r[0] = a[0] - b[0];
  r[1] = a[1] + b[1];
}

/* { dg-final { scan-assembler "\tv?addsubps" } } */
