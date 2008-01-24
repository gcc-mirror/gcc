/* PR middle-end/34934 */

#include <stdarg.h>

typedef struct
{
  int e[1024];
  int f;
} S;

void foo (long *, va_list);

void
bar (long *x, S *y, int z, ...)
{
  int i, j;
  va_list ap;
  va_start (ap, z);
  for (j = y->e[i = 1]; i <= y->f; j = y->e[++i])
    {
      switch (z)
	{
	case 1:
	  if (!(*x & 0x00000020))
	    continue;
	case 3:
	  if (!(*x & 0x00000080))
	    continue;
	case 9:
	  if (!(*x & 0x04000000))
	    continue;
	case 4:
	  if (!(*x & 0x00000200))
	    continue;
	case 8:
	  if (!(*x & 0x00100000))
	    continue;
	case 6:
	  if (!(*x & 0x00000100))
	    continue;
	case 7:
	  if (!(*x & 0x00040000))
	    continue;
	case 10:
	  if (!(*x & 0x00000020)
	      && ((*x & 0x00008000) || (*x & 0x08000000)))
	    continue;
	}
      foo (x, ap);
    }
  va_end (ap);
}
