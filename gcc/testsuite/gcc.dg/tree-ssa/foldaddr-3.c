/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
union a
{
  struct s1
  {
    long long a;
    long long b;
  } s1;
  struct s2
  {
    int c;
    int d;
  } s2;
  struct s3
  {
    unsigned long long e;
    unsigned long long f;
  } s3;
} a;
int *
t ()
{
  return (int *) &a;
}

/* { dg-final { scan-tree-dump "a.s2.c" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
