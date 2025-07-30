/* PR tree-optimization/121190 */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=znver2" { target x86_64-*-* i?86-*-* } } */
/* { dg-require-effective-target mmap } */
/* { dg-require-effective-target vect_early_break } */

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include "tree-vect.h"

#define MAX_COMPARE 5000

__attribute__((noipa))
int diff (uint64_t *restrict p, uint64_t *restrict q)
{
  int i = 0;
  while (i < MAX_COMPARE) {
    if (*(p + i) != *(q + i))
      return i;
    i++;
  }
  return -1;
}

int main ()
{
  check_vect ();

  long pgsz = sysconf (_SC_PAGESIZE);
  if (pgsz == -1) {
    fprintf (stderr, "sysconf failed\n");
    return 0;
  }

  /* Allocate 2 consecutive pages of memory and let p1 and p2 point to the
     beginning of each.  */
  void *mem = mmap (NULL, pgsz * 2, PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    fprintf (stderr, "mmap failed\n");
    return 0;
  }
  uint64_t *p1 = (uint64_t *) mem;
  uint64_t *p2 = (uint64_t *) mem + pgsz / sizeof (uint64_t);

  /* Fill the first page with zeros, except for its last 64 bits.  */
  memset (p1, 0, pgsz);
  *(p2 - 1) = -1;

  /* Make the 2nd page not accessable.  */
  mprotect (p2, pgsz, PROT_NONE);

  /* Calls to diff should not read the 2nd page.  */
  for (int i = 1; i <= 20; i++) {
    if (diff (p2 - i, p1) != i - 1)
      __builtin_abort ();
  }
}

