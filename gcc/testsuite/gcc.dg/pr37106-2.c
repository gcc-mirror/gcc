/* PR c/37106 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-options "-O3 -fpic" { target fpic } } */

#define SIZE 256
float a[SIZE], b[SIZE], c[SIZE];

void non_opt3 (void) __attribute__((__optimize__(1)));

void
not_opt3 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] - c[i];
}

void
opt3 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] + c[i];
}
