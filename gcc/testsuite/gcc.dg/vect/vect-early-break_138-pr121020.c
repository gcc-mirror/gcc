/* PR tree-optimization/121020 */
/* { dg-options "-O3 --vect-cost-model=unlimited" } */
/* { dg-additional-options "-march=znver2" { target x86_64-*-* i?86-*-* } } */
/* { dg-require-effective-target mmap } */
/* { dg-require-effective-target vect_early_break } */

#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include "tree-vect.h"

__attribute__((noipa))
bool equal (uint64_t *restrict p, uint64_t *restrict q, int length)
{
  for (int i = 0; i < length; i++) {
    if (*(p + i) != *(q + i))
      return false;
  }
  return true;
}

int main ()
{
  check_vect ();

  long pgsz = sysconf (_SC_PAGESIZE);
  if (pgsz == -1) {
    fprintf (stderr, "sysconf failed\n");
    return 0;
  }

  /* Allocate a whole page of memory.  */
  void *mem = mmap (NULL, pgsz, PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    fprintf (stderr, "mmap failed\n");
    return 0;
  }
  uint64_t *p1 = (uint64_t *) mem;
  uint64_t *p2 = (uint64_t *) mem + 32;

  /* The first 16 elements pointed to by p1 and p2 are the same.  */
  for (int i = 0; i < 32; i++) {
    *(p1 + i) = 0;
    *(p2 + i) = (i < 16 ? 0 : -1);
  }

  /* All calls to equal should return true.  */
  for (int len = 0; len < 16; len++) {
    if (!equal (p1 + 1, p2 + 1, len))
      __builtin_abort();
  }
}
