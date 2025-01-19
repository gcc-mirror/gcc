/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fno-tree-vectorize -fno-pic" } */
/* { dg-final { scan-assembler-times {(?n)movl[ \t]+.*, p1\.0\+[0-9]*\(,} 1 } } */

int
foo (long indx, long indx2, long indx3, long indx4, long indx5, long indx6, long n, int* q)
{
  static int p1[10000];
  int* p2 = p1 + 1000;
  int* p3 = p1 + 4000;
  int* p4 = p1 + 8000;

  for (long i = 0; i != n; i++)
    {
      /* scan for 	movl	%edi, p1.0+3996(,%rax,4),
	 p1.0+3996 should be propagted into the loop.  */
      p2[indx++] = q[indx++];
      p3[indx2++] = q[indx2++];
      p4[indx3++] = q[indx3++];
    }
  return p1[indx6] + p1[indx5];
}
