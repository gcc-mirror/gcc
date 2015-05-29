/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

void foo(char *);
void my_memcpy (void *q, unsigned int n)
{
  unsigned i;
  char p[1024];
  for (i = 0; i < n; ++i)
    ((char *)p)[i] = ((char *)q)[i];
  foo(p);
}

struct S { int i; int j; };

void my_memcpy2 (void *q, unsigned int n)
{
  unsigned i;
  char p[1024];
  for (i = 0; i < n; ++i)
    ((struct S *)p)[i] = ((struct S *)q)[i];
  foo(p);
}

char p[1024];
void my_memmove (unsigned int n)
{
  unsigned i;
  for (i = 0; i < n; ++i)
    p[i] = p[i+1];
  foo(p);
}


/* { dg-final { scan-tree-dump-times "generated memcpy" 2 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memmove" 1 "ldist" } } */
