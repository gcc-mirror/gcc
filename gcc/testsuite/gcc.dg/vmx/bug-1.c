#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <altivec.h>

#define NPAGES 20
#define NSKIP 10
static vector float big[NPAGES*4096/16] = { {1,1,1,1} };
/* NPAGES pages worth.  */

static int failed;

static void f(vector float *p)
{
  int i = 1;
  p = (vector float *)(((ptrdiff_t)p + 4095) & ~4095);

  i += NSKIP;
  p += NSKIP*4096/16;

  while (i < NPAGES)
    {
      if (!vec_all_eq(*p,((vector float){0,0,0,0})))
	{
	  printf("*p isn't zero at 0x%p, page %d\n", p, i);
	  failed++;
	}
      i++;
      p += 4096/16;
    }
}

int main(void)
{
  f(big);
  if (failed)
    abort ();
  return 0;
}
