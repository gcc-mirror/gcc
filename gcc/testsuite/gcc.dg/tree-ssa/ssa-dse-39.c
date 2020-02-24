/* PR tree-optimization/93683 - ICE on calloc with unused return value
   in ao_ref_init_from_ptr_and_size
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-unused-result -fdump-tree-cddce1" } */

void f0 (int *a)
{
  *a = 0;
  __builtin_calloc (1, 1);
}

void f1 (int *a, unsigned n)
{
  *a = n;
  __builtin_calloc (n, n);
}

/* { dg-final { scan-tree-dump-not "calloc" "cddce1" } } */
