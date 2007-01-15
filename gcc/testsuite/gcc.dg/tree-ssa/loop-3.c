/* A test for strength reduction and induction variable elimination.
   Target is restricted to x86 type architectures, so that we may
   assume something about memory addressing modes.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-fpic" "-fPIC" } { "" } } */
/* { dg-options "-O1 -fno-pic -fno-PIC -fdump-tree-optimized" } */

int arr_base[100];

int foo(int);

void xxx(void)
{
  long iter;

  for (iter = 0; iter < 100; iter++)
    arr_base[iter] = foo (iter);
}

/* Access to arr_base[iter].y should not be strength reduced, since
   we have a memory mode including multiplication by 4.  */

/* { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "step:" 1 "optimized" } } */

/* And original induction variable should be preserved.  */

/* { dg-final { scan-tree-dump-times "int iter" 1 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
