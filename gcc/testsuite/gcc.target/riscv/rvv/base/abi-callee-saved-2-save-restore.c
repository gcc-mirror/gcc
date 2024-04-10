/* { dg-do compile } */
/* { dg-options "-O1 -march=rv64gcv_zfh -mabi=lp64d -msave-restore" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <riscv_vector.h>

void bar1 (vint8m1_t a);
void bar2 ();

/*
** foo1:
**   tail\tbar1
*/
void
foo1 (vint8m1_t a)
{
  bar1 (a);
}

/*
**  foo2:
**    call\tt0,__riscv_save_0
**    csrr\tt0,vlenb
**    sub\tsp,sp,t0
**    vs1r\.v\tv1,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv2,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv3,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv4,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv5,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv6,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv7,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv24,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv25,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv26,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv27,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv28,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv29,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv30,0\(sp\)
**    sub\tsp,sp,t0
**    vs1r\.v\tv31,0\(sp\)
**    call\tbar2
**    csrr\tt0,vlenb
**    vl1re64\.v\tv31,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv30,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv29,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv28,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv27,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv26,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv25,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv24,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv7,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv6,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv5,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv4,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv3,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv2,0\(sp\)
**    add\tsp,sp,t0
**    vl1re64\.v\tv1,0\(sp\)
**    add\tsp,sp,t0
**    tail\t__riscv_restore_0

*/
void
foo2 (vint8m1_t a)
{
  bar2 ();
}

/*
** foo3:
**   call\tt0,__riscv_save_0
**   vl1re8\.v\tv8,0\(a0\)
**   call\tbar1
**   call\tbar2
**   tail\t__riscv_restore_0
*/
void
foo3 (vint8m1_t *a)
{
  bar1 (*a);
  bar2 ();
}
