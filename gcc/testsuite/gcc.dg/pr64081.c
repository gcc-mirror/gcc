/* PR rtl-optimization/64081 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -fdump-rtl-loop2_unroll" } */

long token;
long *data;
long *arr1;
long *arr2;

int main (int argc, const char* argv[])
{
  unsigned long i;
  static long pos, start, count;
  static int dir;

  pos = 0;
  dir = 1;

  for (i = 0; i < argc; i++)
    {
      start = pos;
      for (count = 0; count <= 400; count++)
	{
	  if (token == data[pos])
	    break;
	  if (dir == 1)
	    pos = arr1[pos];
	  else
	    pos = arr2[pos];
	  if (pos == -1)
	    {
	      pos = start;
	      dir = !dir;
	    }
	}
    }
  return pos + dir;
}

/* { dg-final { scan-rtl-dump "loop unrolled 7 times" "loop2_unroll" } } */
/* { dg-final { cleanup-rtl-dump "loop*" } } */
