/* { dg-do compile } */
/* { dg-options "-O1 -march=rv64gcv_zfh -mabi=lp64d -msave-restore" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <riscv_vector.h>

void bar (int8_t *data);

/*
** foo1:
**   call\tt0,__riscv_save_0
**   csrr\tt0,vlenb
**   sub\tsp,sp,t0
**   vs1r\.v\tv1,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv2,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv3,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv4,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv5,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv6,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv7,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv24,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv25,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv26,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv27,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv28,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv29,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv30,0\(sp\)
**   sub\tsp,sp,t0
**   vs1r\.v\tv31,0\(sp\)
**   addi\tsp,sp,-1024
**   mv\ta0,sp
**   call\tbar
**   addi\tsp,sp,1024
**   csrr\tt0,vlenb
**   vl1re64\.v\tv31,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv30,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv29,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv28,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv27,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv26,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv25,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv24,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv7,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv6,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv5,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv4,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv3,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv2,0\(sp\)
**   add\tsp,sp,t0
**   vl1re64\.v\tv1,0\(sp\)
**   add\tsp,sp,t0
**   tail\t__riscv_restore_0
*/
void
foo1 (vint8m1_t a)
{
  int8_t data[1024];
  bar (data);
}
