/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fdump-tree-cddce1" } */

typedef int v8si __attribute__((vector_size(32)));
typedef float v4sf __attribute__((vector_size(16)));

void low (v4sf *dst, v8si *srcp)
{
  v8si src = *srcp;
  *dst = (v4sf) { src[0], src[1], src[2], src[3] };
}

void high (v4sf *dst, v8si *srcp)
{
  v8si src = *srcp;
  *dst = (v4sf) { src[4], src[5], src[6], src[7] };
}

void even (v4sf *dst, v8si *srcp)
{
  v8si src = *srcp;
  *dst = (v4sf) { src[0], src[2], src[4], src[6] };
}

void odd (v4sf *dst, v8si *srcp)
{
  v8si src = *srcp;
  *dst = (v4sf) { src[1], src[3], src[5], src[7] };
}

/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 4 "cddce1" } } */
/* Four conversions, on the smaller vector type, to not convert excess
   elements.  */
/* { dg-final { scan-tree-dump-times " = \\\(vector\\\(4\\\) float\\\)" 4 "cddce1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 3 "cddce1" } } */
/* Ideally highpart extraction would elide the VEC_PERM_EXPR as well.  */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 2 "cddce1" { xfail *-*-* } } } */
