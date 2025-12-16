/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

/* Check if we handle alloca size zero cases.  */
int a, b, c;
void foo (void)
{
  int *d = __builtin_alloca (0);
  for (;; a = b)
    d[c] = *d;
}

/* We check if and only if we tag the 'd' variable.  */
/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {stg\t...?, \[sp\]\n} 1 } } */
