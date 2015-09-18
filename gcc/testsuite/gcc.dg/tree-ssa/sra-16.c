/* Verify that SRA total scalarization works on records containing arrays.  */
/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-release_ssa --param sra-max-scalarization-size-Ospeed=16" } */

extern void abort (void);

struct S
{
  long zilch[0];
  char c;
  int i;
  unsigned short f3, f4;
};


int __attribute__ ((noinline))
foo (struct S *p)
{
  struct S l;

  l = *p;
  l.i++;
  l.f3++;
  *p = l;
}

int
main (int argc, char **argv)
{
  struct S a = { { }, 0, 4, 0, 0};
  foo (&a);
  if (a.i != 5 || a.f3 != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "l;" 0 "release_ssa" } } */
