/* { dg-options "-O2 -Wall" } */

#include <arm_sve.h>

svuint64_t bar(svbool_t pg, const uint64_t *addr) {
  return svget2(svld2_u64(pg, addr), 0);
}
