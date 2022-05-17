/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -fdump-tree-cddce1" } */

typedef int v4si __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));

void low (v2si *dst, v4si *srcp)
{
  v4si src = *srcp;
  *dst = (v2si) { src[0], src[1] };
}

void high (v2si *dst, v4si *srcp)
{
  v4si src = *srcp;
  *dst = (v2si) { src[2], src[3] };
}

void even (v2si *dst, v4si *srcp)
{
  v4si src = *srcp;
  *dst = (v2si) { src[0], src[2] };
}

void odd (v2si *dst, v4si *srcp)
{
  v4si src = *srcp;
  *dst = (v2si) { src[1], src[3] };
}

/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 4 "cddce1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 3 "cddce1" { xfail *-*-* } } } */
/* Ideally highpart extraction would elide the permutation as well.  */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 2 "cddce1" } } */
