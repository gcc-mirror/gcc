/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 " } */
/* { dg-final { scan-assembler-times "lxvd2x" 2 } } */
/* { dg-final { scan-assembler-times "stxvd2x" 1 } } */
/* { dg-final { scan-assembler-times "xxpermdi" 3 } } */

/* Verify that swap optimization works correctly for a VSX direct splat.
   The three xxpermdi's that are generated correspond to two splats
   and the __builtin_vsx_xxpermdi.  */

int printf (const char *__restrict __format, ...);
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

double s1[] = {2134.3343, 6678.346};
double s2[] = {41124.234, 6678.346};
long long dd[] = {1, 2}, d[2];
union{long long l[2]; double d[2];} e;

void
foo ()
{
  __m128d source1, source2, dest;
  __m128d a, b, c;

  e.d[1] = s1[1];
  e.l[0] = !__builtin_isunordered(s1[0], s2[0]) 
    && s1[0] == s2[0] ? -1 : 0;
  source1 = __builtin_vec_vsx_ld (0, s1);
  source2 = __builtin_vec_vsx_ld (0, s2);
  a = __builtin_vec_splat (source1, 0);
  b = __builtin_vec_splat (source2, 0);
  c = (__m128d)__builtin_vec_cmpeq (a, b);
  dest = __builtin_vsx_xxpermdi (source1, c, 1);
  *(__m128d *)d = dest;
}
