#include <limits.h>

#ifdef __unix__ /* ??? Is that good enough? */
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON MAP_FILE
#endif
#endif
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
#ifndef MAP_FIXED
#define MAP_FIXED 0
#endif
#endif

#define MAP_START (void *)0x7fff8000
#define MAP_LEN 0x10000

#define OFFSET (MAP_LEN/2 - 2 * sizeof (int));

f (int *p, int **q)
{
  int i;
  for (i = 0; i < 40; i++)
    {
      *q++ = &p[i];
    }
}

main ()
{
#ifdef MAP_ANON
  void *p;
  int *q[40];
  int dev_zero;

  dev_zero = open ("/dev/zero", O_RDONLY);
  /* -1 is OK when we have MAP_ANON; else mmap will flag an error.  */
  if (INT_MAX != 0x7fffffffL || sizeof (char *) != sizeof (int))
    exit (0);
  p = mmap (MAP_START, MAP_LEN, PROT_READ|PROT_WRITE,
	    MAP_ANON|MAP_FIXED|MAP_PRIVATE, dev_zero, 0);
  if (p != (void *)-1)
    {
      p = (char *)p + OFFSET;
      q[39] = 0;
      f (p, q);
      if (q[39] != (int *)p + 39)
	abort ();
    }
#endif
  exit (0);
}
