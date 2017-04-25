/* Machine description pattern tests.  */

/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-do run { target { s390_useable_hw } } } */

#include <stdio.h>

struct
{
#ifdef __s390xx__
  __int128 dummy128;
  __int128 mem128;
#endif
  long long dummy64;
  long long mem64;
  int dummy32;
  int mem32;
  short mem16l;
  short mem16h;
  char mem8ll;
  char mem8lh;
  char mem8hl;
  char mem8hh;
} mem_s;

#define TYPE char
#define FN(SUFFIX) f8 ## SUFFIX
#define FNS(SUFFIX) "f8" #SUFFIX
#include "atomic_compare_exchange-1.inc"

#define TYPE short
#define FN(SUFFIX) f16 ##SUFFIX
#define FNS(SUFFIX) "f16" #SUFFIX
#include "atomic_compare_exchange-1.inc"

#define TYPE int
#define FN(SUFFIX) f32 ## SUFFIX
#define FNS(SUFFIX) "f32" #SUFFIX
#include "atomic_compare_exchange-1.inc"

#define TYPE long long
#define FN(SUFFIX) f64 ## SUFFIX
#define FNS(SUFFIX) "f64" #SUFFIX
#include "atomic_compare_exchange-1.inc"

#ifdef __s390xx__
#define TYPE __int128
#define FN(SUFFIX) f128 ## SUFFIX
#define FNS(SUFFIX) "f128" #SUFFIX
#include "atomic_compare_exchange-1.inc"
#endif

int main(void)
{
  int err_count = 0;
  int i;

  for (i = -1; i <= 2; i++)
    {
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f8_validate(&mem_s.mem8ll, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f8_validate(&mem_s.mem8lh, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f8_validate(&mem_s.mem8hl, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f8_validate(&mem_s.mem8hh, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f16_validate(&mem_s.mem16l, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f16_validate(&mem_s.mem16h, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f32_validate(&mem_s.mem32, i, 1);
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f64_validate(&mem_s.mem64, i, 1);
#ifdef __s390xx__
      __builtin_memset(&mem_s, 0x99, sizeof(mem_s));
      err_count += f128_validate(&mem_s.mem128, i, 1);
#endif
    }

  return err_count;
}
