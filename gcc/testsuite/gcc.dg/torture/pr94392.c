/* { dg-do compile } */
/* { dg-skip-if "finite loops" { *-*-* } { "-ffinite-loops" } } */
/* { dg-skip-if "LTO optimizes the test" { *-*-* } { "-flto" } } */
/* { dg-additional-options "-fdump-tree-optimized" } */

int a, b;

int
main()
{
  while (1)
    {
      /* Try really hard.  */
      if (a != b)
	return 1;
    }
  return 0;
}

/* ISO C does not guarantee forward progress like C++ does so we
   cannot assume the loop is finite and optimize it to return 1.  */
/* { dg-final { scan-tree-dump "if" "optimized" } } */
