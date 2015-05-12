/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4 -mtune=generic" } */

/* We need SSE4 so the backend recognizes a { 0, 5, 2, 7 } constant
   permutation as supported as the vectorizer wants to generate

     vect__6.10_24 = vect__3.6_20 - vect__5.9_23;
     vect__6.11_25 = vect__3.6_20 + vect__5.9_23;
     _26 = VEC_PERM_EXPR <vect__6.10_24, vect__6.11_25, { 0, 5, 2, 7 }>;

   See also the ??? comment about using and/andn/or in expand_vec_perm_blend
   for non-SSE4 targets.  */

void testf (float * __restrict__ p, float * __restrict q)
{
  p[0] = p[0] - q[0];
  p[1] = p[1] + q[1];
  p[2] = p[2] - q[2];
  p[3] = p[3] + q[3];
}

/* { dg-final { scan-assembler "addsubps" } } */
