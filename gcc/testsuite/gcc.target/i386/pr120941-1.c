/* { dg-do compile } */
/* { dg-options "-Ofast -march=x86-64-v3" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**bar:
**.LFB[0-9]+:
**...
**	vbroadcastsd	.LC4\(%rip\), %ymm2
**	leal	2\(%rbx\), %eax
**	vbroadcastsd	.LC2\(%rip\), %ymm4
**	negl	%eax
**...
*/

extern void foo (int);

enum { N_CELL_ENTRIES1 = 2 }
typedef LBM_Grid1[64];
enum { N_CELL_ENTRIES2 = 2 }
typedef LBM_Grid2[64];
LBM_Grid1 grid1;
LBM_Grid2 grid2;
extern int n;

void
LBM_handleInOutFlow()
{
  int i, j;
  for (; i; i += 2)
    {
      for (j = 0; j < n; j++)
	{
	  grid1[i] = 1.0 / 36.0 * i;
	  grid2[i] = 1.0 / 36.0 * i;
	}
    }
}

int main_t;
void
bar (void)
{
  for (; main_t; main_t++) {
    LBM_handleInOutFlow();
    foo (main_t);
  }
}
