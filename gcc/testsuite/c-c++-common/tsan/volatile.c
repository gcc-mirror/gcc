/* { dg-options "--param=tsan-distinguish-volatile=1 -fdump-tree-optimized" } */

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int32_t Global4;
volatile int32_t VolatileGlobal4;
volatile int64_t VolatileGlobal8;

static int nvolatile_reads;
static int nvolatile_writes;

#ifdef __cplusplus
extern "C" {
#endif

__attribute__((no_sanitize_thread))
void __tsan_volatile_read4(void *addr) {
  assert(addr == &VolatileGlobal4);
  nvolatile_reads++;
}
__attribute__((no_sanitize_thread))
void __tsan_volatile_write4(void *addr) {
  assert(addr == &VolatileGlobal4);
  nvolatile_writes++;
}
__attribute__((no_sanitize_thread))
void __tsan_volatile_read8(void *addr) {
  assert(addr == &VolatileGlobal8);
  nvolatile_reads++;
}
__attribute__((no_sanitize_thread))
void __tsan_volatile_write8(void *addr) {
  assert(addr == &VolatileGlobal8);
  nvolatile_writes++;
}

#ifdef __cplusplus
}
#endif

__attribute__((no_sanitize_thread))
static void check() {
  assert(nvolatile_reads == 4);
  assert(nvolatile_writes == 4);
}

int main() {
  Global4 = 1;

  VolatileGlobal4 = 1;
  Global4 = VolatileGlobal4;
  VolatileGlobal4 = 1 + VolatileGlobal4;

  VolatileGlobal8 = 1;
  Global4 = (int32_t)VolatileGlobal8;
  VolatileGlobal8 = 1 + VolatileGlobal8;

  check();
  return 0;
}

// { dg-final { scan-tree-dump-times "__tsan_volatile_read4 \\(&VolatileGlobal4" 2 "optimized" } }
// { dg-final { scan-tree-dump-times "__tsan_volatile_read8 \\(&VolatileGlobal8" 2 "optimized" } }
// { dg-final { scan-tree-dump-times "__tsan_volatile_write4 \\(&VolatileGlobal4" 2 "optimized" } }
// { dg-final { scan-tree-dump-times "__tsan_volatile_write8 \\(&VolatileGlobal8" 2 "optimized" } }
