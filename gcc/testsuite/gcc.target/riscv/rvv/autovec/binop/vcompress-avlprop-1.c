/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -O3 -mrvv-vector-bits=zvl -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-skip-if "" { *-*-* } { "-mrvv-max-lmul=dynamic" } } */
#define MAX     10

struct s { struct s *n; } *p;
struct s ss;
struct s sss[MAX];

/*
** build_linked_list:
**   ...
**   vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*tu,\s*ma
**   ...
**   vcompress\.vm\s+v[0-9]+,\s*v[0-9]+,\s*v0
**   ...
**   vcompress\.vm\s+v[0-9]+,\s*v[0-9]+,\s*v0
**   vsetivli\s+zero,\s*2,\s*e64,\s*m1,\s*ta,\s*ma
**   ...
*/
void
build_linked_list ()
{
  int i;
  struct s *next;

  p = &ss;
  next = p;

  for (i = 0; i < MAX; i++) {
      next->n = &sss[i];
      next = next->n;
  }

  next->n = 0;
}
