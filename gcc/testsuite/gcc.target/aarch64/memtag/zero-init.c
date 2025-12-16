/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

void use (int *);

void foo (void)
{
  int x = 0;
  use (&x);
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {stg\t...?, \[sp, 16\]\n} 2 } } */

