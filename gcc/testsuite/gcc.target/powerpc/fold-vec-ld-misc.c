/* Verify that overloaded built-ins for vec_ld with
   structure pointer / double inputs produce the right code.  */

/* This test is to ensure that when a cast is associated with arg1 on a
   call to vec_ld (arg0, arg1), that the arg1 type is properly handled
   through the gimple folding code.
   We want something like this:
	D.2736 = MEM[(voidD.44 *)D.2739];
   We specifically do not want 'struct S' showing up:
	D.3212 = MEM[(struct S *)D.3215];
*/

/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2 -fdump-tree-gimple" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>
#include <stdio.h>

struct S {
  vector int *i1,*i2;
  vector long long *ll1;
  vector double *vd1;
  vector double *vd2;
  vector double *vd3;
  vector double *vd4;
};

vector double
testld_struct1 (long long ll1, struct S *p)
{
  return __builtin_altivec_lvx_v2df (ll1, (double *)p);
}

vector double
testld_struct1b (long long ll1, struct S *p)
{
  return vec_ld (ll1, (vector double *)p);
}

vector double
testld_struct2 (struct S *p)
{
  return vec_ld (16, (vector double *)p);
}

vector double
testld_struct3 (struct S *p)
{
  return vec_ld (16, (vector double *)p->vd2);
}

// We do not want the "struct S" reference to show up.
/* { dg-final { scan-tree-dump-times "MEM\[\(struct S *\)D.\[0-9\]+\]" 0 "gimple" } } */
