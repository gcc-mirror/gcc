/* { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-Wopenacc-kernels-annotate-loops" } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-do compile } */

/* Test for accepting annotation on loops that have various forms of
   loop-invariant expressions in their end test.  */

extern const int x;
extern int g (int) __attribute__ ((const));

void f (float *a, float *b, int n)
{

  int j;
#pragma acc kernels
  {
    /* Reversed form of comparison.  */
    for (int i = 0; n >= i; i++)
      a[i] = b[i];
    
    /* Constant function call.  */
    for (int i = 0; i < g(n); i++)
      a[i] = b[i];

    /* Constant global variable.  */
    for (int i = 0; i < x; i++)
      a[i] = b[i];

    /* Complicated expression involving conditionals, etc. */
    for (int i = 0; i < ((x == 4) ? (n << 2) : (n << 3)); i++)
      a[i] = b[i];

    /* Reference to a local variable not modified in the loop.  */
    j = ((x == 4) ? (n << 2) : (n << 3));
    for (int i = 0; i < j; i++)
      a[i] = b[i];
  }
}

/* { dg-final { scan-tree-dump-times "acc loop auto" 5 "original" } } */
