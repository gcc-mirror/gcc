/* { dg-do run } */
/* { dg-options "-O2 -fsplit-loops -fdump-tree-lsplit-details" } */
/* { dg-require-effective-target int32plus } */

#ifdef __cplusplus
extern "C" void abort (void);
#else
extern void abort (void);
#endif

typedef struct {
  int n;
} region_t;

void set (region_t *region) __attribute__((noinline));
void doit (region_t *beg, region_t *end, region_t *limit)
  __attribute__((noinline));

region_t regions[10];

void
set (region_t *region) {
  region->n = 1;
}

void
doit (region_t *beg, region_t *end, region_t *limit) {
  for (region_t *cur = beg; cur < end; cur++) {
    if (cur < limit) {
      set(cur);
    }
  }
}

int
main (void) {
  doit(&regions[0], &regions[2], &regions[10]);
  if (regions[1].n != 1)
    abort();
}
