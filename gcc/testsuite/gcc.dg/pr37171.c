/* PR c/37171 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int f1 (void) __attribute__((const));
unsigned int f2 (void) __attribute__((__const));
unsigned int f3 (void) __attribute__((__const__));

unsigned int f4 (void)
{
  return f1 () + f1 () + f1 () + f1 ()
	 + f2 () + f2 () + f2 () + f2 ()
	 + f3 () + f3 () + f3 () + f3 ();
}

/* { dg-final { scan-tree-dump-times "= f1 \\(\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= f2 \\(\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= f3 \\(\\)" 1 "optimized" } } */
