/* PR rtl-optimization/87485 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fschedule-insns -fno-guess-branch-probability -fno-isolate-erroneous-paths-dereference -fno-omit-frame-pointer -fno-split-wide-types -fno-tree-ccp -fno-tree-sra" } */
/* { dg-additional-options "-fstack-protector-strong" { target fstack_protector } } */

int *a;

int
foo (__int128 x, int y, int z)
{
  __int128 b;
  *a = ((!!y ? y : x) * y | x) * 2;
  if (z == 0)
    {
      unsigned int c = 1;
      __int128 *d = &b;
      for (*a = 0; *a < 1; *a += y)
	;
      *a += b < (c / 0);	/* { dg-warning "division by zero" } */
      goto l;
 m:
      while (b < 1)
	;
      ++*a;
    }
  goto m;
 l:
  return 0;
}
