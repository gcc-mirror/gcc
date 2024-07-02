/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-veclower2" } */
/* PR tree-optimization/110473 */
/* convertvector lowering should produce directly the casts
   rather than lower them to scalar.  */

typedef unsigned int v4si __attribute__ ((vector_size (4*sizeof(int))));
typedef unsigned short v4hi __attribute__ ((vector_size (4*sizeof(short))));

v4si f(v4si a, v4si b)
{
  v4hi t = __builtin_convertvector (a, v4hi);
  v4si t1 = __builtin_convertvector (t, v4si);
  return t1;
}

/* { dg-final { scan-assembler-times "\txtn\t" 1 } } */
/* { dg-final { scan-assembler-times "\tuxtl\t" 1 } } */
/* { dg-final { scan-tree-dump-times " = .v4hi. a_" 1 "veclower21" } } */
/* { dg-final { scan-tree-dump-times " = .v4si. " 1 "veclower21" } } */
