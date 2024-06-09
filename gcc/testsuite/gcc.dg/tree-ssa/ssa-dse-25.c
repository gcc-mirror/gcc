/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details -w" } */

char z[32];


int
foo(void)
{
  __builtin_memset (z, 0, 16);
  __builtin_memset (z+8, 0, 24);
}

/* { dg-final { scan-tree-dump "memset .&z, 0, 8." "dse1" } } */


