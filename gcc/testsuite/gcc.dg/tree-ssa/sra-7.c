/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

typedef struct { char f[4]; } __attribute__((aligned (4))) s;

void a(s *s1, s *s2)
{
  *s1 = *s2;
}

/* Struct copies should not be split into members.  */
/* { dg-final { scan-tree-dump "\\\*s1_.\\\(D\\\) = \\\*s2"  "optimized"} } */
