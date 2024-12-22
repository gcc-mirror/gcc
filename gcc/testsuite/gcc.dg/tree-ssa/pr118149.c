/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop4-details -Wno-psabi" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

float *fastconv_parse_dst;

void fastconv_parse ()
{
  float r3k = fastconv_parse_dst[1] - fastconv_parse_dst[3],
        i0k = fastconv_parse_dst[4] + fastconv_parse_dst[6],
        i1k = fastconv_parse_dst[4] - fastconv_parse_dst[6],
        i2k = fastconv_parse_dst[5] + fastconv_parse_dst[7];
  fastconv_parse_dst[1] = fastconv_parse_dst[0];
  fastconv_parse_dst[4] = fastconv_parse_dst[5] = i0k - i2k;
  fastconv_parse_dst[6] = fastconv_parse_dst[7] = i1k + r3k;
}

/* { dg-final { scan-tree-dump "Vec perm simplify sequences have been blended" "forwprop4" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 0, 0, 6, 6 }" "forwprop4" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 1, 1, 7, 7 }" "forwprop4" { target { i?86-*-* x86_64-*-* } } } } */
