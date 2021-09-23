/* { dg-do compile } */
/* { dg-options "-msse2 -mno-sse4.1 -O2" } */
/* { dg-final { scan-assembler-times {(?n)(?:mov|psrldq).*%xmm[0-9]} 12 } } */
/* { dg-final { scan-assembler-not "pxor" } } */

typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));

v2df
foo_v2df (v2df x)
{
  return __builtin_shuffle (x, (v2df) { 0, 0 }, (v2di) {0, 2});
}

v2df
foo_v2df_l (v2df x)
{
  return __builtin_shuffle ((v2df) { 0, 0 }, x, (v2di) {3, 1});
}

v2di
foo_v2di (v2di x)
{
  return __builtin_shuffle (x, (v2di) { 0, 0 }, (v2di) {0, 3});
}

v2di
foo_v2di_l (v2di x)
{
  return __builtin_shuffle ((v2di) { 0, 0 }, x, (v2di) {3, 0});
}

v4sf
foo_v4sf (v4sf x)
{
  return __builtin_shuffle (x, (v4sf) { 0, 0, 0, 0 }, (v4si) {0, 1, 4, 5});
}

v4sf
foo_v4sf_l (v4sf x)
{
  return __builtin_shuffle ((v4sf) { 0, 0, 0, 0 }, x, (v4si) {4, 5, 3, 1});
}

v4si
foo_v4si (v4si x)
{
  return __builtin_shuffle (x, (v4si) { 0, 0, 0, 0 }, (v4si) {0, 1, 6, 7});
}

v4si
foo_v4si_l (v4si x)
{
  return __builtin_shuffle ((v4si) { 0, 0, 0, 0 }, x, (v4si) {4, 5, 1, 2});
}

v8hi
foo_v8hi (v8hi x)
{
  return __builtin_shuffle (x, (v8hi) { 0, 0, 0, 0, 0, 0, 0, 0 },
			       (v8hi) { 0, 1, 2, 3, 8, 12, 10, 13 });
}

v8hi
foo_v8hi_l (v8hi x)
{
  return __builtin_shuffle ((v8hi) { 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8hi) { 8, 9, 10, 11, 7, 6, 5, 4 });
}

v16qi
foo_v16qi (v16qi x)
{
  return __builtin_shuffle (x, (v16qi) { 0, 0, 0, 0, 0, 0, 0, 0,
					 0, 0, 0, 0, 0, 0, 0, 0 },
			       (v16qi) {0, 1, 2, 3, 4, 5, 6, 7,
					16, 24, 18, 26, 20, 28, 22, 30 });
}

v16qi
foo_v16qi_l (v16qi x)
{
  return __builtin_shuffle ((v16qi) { 0, 0, 0, 0, 0, 0, 0, 0,
				       0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v16qi) { 16, 17, 18, 19, 20, 21, 22, 23,
				      15, 0, 13, 2, 11, 4, 9, 6 });
}
