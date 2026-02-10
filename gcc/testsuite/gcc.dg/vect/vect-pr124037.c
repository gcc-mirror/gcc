/* PR tree-optimization/124037 */
/* { dg-require-effective-target mmap } */
/* { dg-require-effective-target vect_early_break } */
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define MAX 65536

typedef struct {
  uint64_t a;
  uint64_t b;
  uint32_t flag;
  uint32_t pad;
} Data;

int __attribute__ ((noinline))
foo (Data *ptr) {
  if (ptr == NULL)
    return -1;

  int cnt;
  for (cnt = 0; cnt < MAX; cnt++) {
    if (ptr->flag == 0)
      break;
    ptr++;
  }
  return cnt;
}

int main() {
  long pgsz = sysconf (_SC_PAGESIZE);
  if (pgsz == -1) {
    fprintf (stderr, "sysconf failed\n");
    return 0;
  }

  /* Allocate 2 consecutive pages.  */
  void *mem = mmap (NULL, pgsz * 2, PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    fprintf (stderr, "mmap failed\n");
    return 0;
  }

  memset (mem, 1, pgsz);
  uint8_t *ptr = (uint8_t*) mem + pgsz;
  memset (ptr - 16, 0, 16);

  mprotect (ptr, pgsz, PROT_NONE);
  Data *data = (Data *) (ptr - 80);
  __builtin_printf("%d\n", foo(data));
}

/* { dg-final { scan-tree-dump "missed:   not vectorized: relevant stmt not supported: _\[0-9\]+ = ptr_\[0-9\]+->flag;" "vect" } } */
/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
