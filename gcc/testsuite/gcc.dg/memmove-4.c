/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef int w __attribute__((mode(word)));

void b(char *a, char *b, int i)
{
  __builtin_memmove (&a[i], &b[i], sizeof(w));
}

/* { dg-final { scan-tree-dump-not "memmove" "optimized" { xfail { ! non_strict_align } } } } */
