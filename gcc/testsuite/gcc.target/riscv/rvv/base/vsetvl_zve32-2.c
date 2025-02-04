/* { dg-do compile } */
/* { dg-options "-march=rv64g_zve32x_zvl128b -mabi=lp64d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned int V2SI __attribute__((vector_size(8)));

V2SI v1, v2;

/* Make sure we won't use mf2 mode even vector register is OK to hold for
   ELEN=32.  */
void foo1()
{
/*
** foo1:
**      ...
**      vsetivli	zero,2,e32,m1,ta,ma
**      ...
**      vle32\.v	v[0-9]+,0\([a-x][0-9]+\)
**      ...
**      vse32\.v	v[0-9]+,0\([a-x][0-9]+\)
**      ...
**      ret
*/
  v1 = v2;
}
