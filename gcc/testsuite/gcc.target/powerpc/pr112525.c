/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

typedef struct teststruct
{
  double d;
  int arr[15]; 
} teststruct;

void
foo (teststruct p)
{
}

/* { dg-final { scan-assembler-not {\mstd\M} } } */
