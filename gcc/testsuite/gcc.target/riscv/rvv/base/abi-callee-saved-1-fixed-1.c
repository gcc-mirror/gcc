/* { dg-do compile } */
/* { dg-options "-O1 -march=rv64gczve32x -mabi=lp64d -mrvv-vector-bits=zvl" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <riscv_vector.h>

void bar (int8_t *data);

/*
** foo1:
**   addi\tsp,sp,-16
**   sd\tra,8\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv1,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv2,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv3,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv4,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv5,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv6,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv7,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv24,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv25,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv26,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv27,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv28,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv29,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv30,0\(sp\)
**   addi\tsp,sp,-4
**   vs1r\.v\tv31,0\(sp\)
**   addi\tsp,sp,-1028
**   mv\ta0,sp
**   call\tbar
**   addi\tsp,sp,1028
**   vl1re32\.v\tv31,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv30,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv29,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv28,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv27,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv26,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv25,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv24,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv7,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv6,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv5,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv4,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv3,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv2,0\(sp\)
**   addi\tsp,sp,4
**   vl1re32\.v\tv1,0\(sp\)
**   addi\tsp,sp,4
**   ld\tra,8\(sp\)
**   addi\tsp,sp,16
**   jr\tra
*/
void
foo1 (vint8m1_t a)
{
  int8_t data[1024];
  bar (data);
}
