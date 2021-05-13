/* { dg-do compile } */
/* { dg-options "-mavx -mno-avx512f -O2" } */
/* { dg-final { scan-assembler-times {(?n)vmov[a-z0-9]*[ \t]*%xmm[0-9]} 12 } } */
/* { dg-final { scan-assembler-not "pxor" } } */

typedef float v8sf __attribute__((vector_size(32)));
typedef double v4df __attribute__ ((vector_size (32)));
typedef long long v4di __attribute__((vector_size(32)));
typedef int v8si __attribute__((vector_size(32)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef char v32qi __attribute__ ((vector_size (32)));

v4df
foo_v4df (v4df x)
{
  return __builtin_shuffle (x, (v4df) { 0, 0, 0, 0 }, (v4di) { 0, 1, 4, 5 });
}

v4df
foo_v4df_l (v4df x)
{
  return __builtin_shuffle ((v4df) { 0, 0, 0, 0 }, x, (v4di) { 4, 5, 1, 2 });
}

v4di
foo_v4di (v4di x)
{
  return __builtin_shuffle (x, (v4di) { 0, 0, 0, 0 }, (v4di) { 0, 1, 4, 7 });
}

v4di
foo_v4di_l (v4di x)
{
  return __builtin_shuffle ((v4di) { 0, 0, 0, 0 }, x, (v4di) { 4, 5, 3, 1 });
}

v8sf
foo_v8sf (v8sf x)
{
  return __builtin_shuffle ((v8sf) { 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8si) { 8, 9, 10, 11, 0, 1, 2, 3 });
}

v8sf
foo_v8sf_l (v8sf x)
{
  return __builtin_shuffle (x, (v8sf) { 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8si) { 0, 1, 2, 3, 8, 9, 10, 11 });
}

v8si
foo_v8si (v8si x)
{
  return __builtin_shuffle (x, (v8si) { 0, 0, 0, 0, 0, 0, 0, 0 },
			    (v8si) { 0, 1, 2, 3, 13, 12, 11, 15 });
}

v8si
foo_v8si_l (v8si x)
{
  return __builtin_shuffle ((v8si) { 0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v8si) { 8, 9, 10, 11, 7, 6, 5, 4 });
}

v16hi
foo_v16hi (v16hi x)
{
  return __builtin_shuffle (x, (v16hi)  { 0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0, 0, 0, 0 },
			       (v16hi) { 0, 1, 2, 3, 4, 5, 6, 7,
					 24, 17, 26, 19, 28, 21, 30, 23 });
}

v16hi
foo_v16hi_l (v16hi x)
{
  return __builtin_shuffle ((v16hi)  { 0, 0, 0, 0, 0, 0, 0, 0,
				       0, 0, 0, 0, 0, 0, 0, 0 }, x,
			    (v16hi) { 16, 17, 18, 19, 20, 21, 22, 23,
				      15, 0, 13, 2, 11, 4, 9, 6 });
}

v32qi
foo_v32qi (v32qi x)
{
  return __builtin_shuffle (x, (v32qi) { 0, 0, 0, 0, 0, 0, 0, 0,
					 0, 0, 0, 0, 0, 0, 0, 0,
					 0, 0, 0, 0, 0, 0, 0, 0,
					 0, 0, 0, 0, 0, 0, 0, 0 },
			       (v32qi) { 0, 1, 2, 3, 4, 5, 6, 7,
					 8, 9, 10, 11, 12, 13, 14, 15,
					 32, 49, 34, 58, 36, 53, 38, 39,
					 40, 60, 42, 43, 63, 45, 46, 47 });
}

v32qi
foo_v32qi_l (v32qi x)
{
  return __builtin_shuffle ((v32qi) { 0, 0, 0, 0, 0, 0, 0, 0,
				      0, 0, 0, 0, 0, 0, 0, 0,
				      0, 0, 0, 0, 0, 0, 0, 0,
				      0, 0, 0, 0, 0, 0, 0, 0 }, x,
			     (v32qi) { 32, 33, 34, 35, 36, 37, 38, 39,
				       40, 41, 42, 43, 44, 45, 46, 47,
				       31, 0, 29, 2, 27, 4, 25, 6,
				       23, 8, 21, 10, 19, 12, 17, 14 });
}
