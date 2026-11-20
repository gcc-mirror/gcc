/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define OFF (1ULL << 61)
void g (char *p)
{
  p[0] = 1;                              /* live */
  __builtin_memset (p + OFF, 7, 1000);   /* does not touch p[0] */
}

/* { dg-final { scan-tree-dump " = 1;" "optimized" } } */
