/* PR rtl-optimization/33673 */
/* { dg-do compile } */
/* { dg-options "-Os -fno-forward-propagate -fno-guess-branch-probability -fno-move-loop-invariants -fno-tree-dominator-opts -fno-tree-loop-optimize" } */

extern int f1 (int);
extern int f2 (int);

extern int *a;

static void
find_reg (int n)
{
  int i, pass;
  unsigned int used[2], used1[2];

  int c = n ? f1 (a[1]) : f2 (a[1]);

  for (i = 64, pass = 0; pass <= 1 && i >= 64; pass++)
    {
      if (pass == 1)
	{
	  unsigned int *scan_tp_ = used;
	  unsigned int *scan_fp_ = used1;
	  int j;
	  for (j = 0; j < 2; j++)
	    *scan_tp_++ = *scan_fp_++;
	}
      for (i = 0; i < 64; i++)
	{
	  int regno = i;
	  if (n == 0)
	    if (i == regno)
	      break;
	}
    }
}

void
global_alloc ()
{
  find_reg (0);
  find_reg (1);
}
