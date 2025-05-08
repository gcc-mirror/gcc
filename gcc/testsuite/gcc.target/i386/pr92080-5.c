/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 3 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastsd" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastss" 1 } } */

typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));
typedef long long v8di __attribute__((vector_size(64)));
typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));
typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef short v32hi __attribute__((vector_size(64)));
typedef char v16qi __attribute__((vector_size(16)));
typedef char v32qi __attribute__((vector_size(32)));
typedef char v64qi __attribute__((vector_size(64)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));
typedef double v2df __attribute__((vector_size(16)));
typedef double v4df __attribute__((vector_size(32)));
typedef double v8df __attribute__((vector_size(64)));

extern v16qi b1;
extern v8hi h1;
extern v4si s1;
extern v2di l1;
extern v4sf f1;
extern v2df d1;
extern v32qi b2;
extern v16hi h2;
extern v8si s2;
extern v4di l2;
extern v8sf f2;
extern v4df d2;
extern v64qi b3;
extern v32hi h3;
extern v16si s3;
extern v8di l3;
extern v16sf f3;
extern v8df d3;

void
foo1 ()
{
  b1 = __extension__(v16qi){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
  b2 = __extension__(v32qi){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
  b3 = __extension__(v64qi){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
}

void
foo2 ()
{
  h1 = __extension__(v8hi){34, 34, 34, 34, 34, 34, 34, 34};
  h2 = __extension__(v16hi){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
  h3 = __extension__(v32hi){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
}

void
foo3 ()
{
  s1 = __extension__(v4si){34, 34, 34, 34};
  s2 = __extension__(v8si){34, 34, 34, 34, 34, 34, 34, 34};
  s3 = __extension__(v16si){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
}

void
foo4 ()
{
  l1 = __extension__(v2di){34, 34};
  l2 = __extension__(v4di){34, 34, 34, 34};
  l3 = __extension__(v8di){34, 34, 34, 34, 34, 34, 34, 34};
}

void
foo5 ()
{
  f1 = __extension__(v4sf){34, 34, 34, 34};
  f2 = __extension__(v8sf){34, 34, 34, 34, 34, 34, 34, 34};
  f3 = __extension__(v16sf){34, 34, 34, 34, 34, 34, 34, 34,
			    34, 34, 34, 34, 34, 34, 34, 34};
}

void
foo6 ()
{
  d1 = __extension__(v2df){34, 34};
  d2 = __extension__(v4df){34, 34, 34, 34};
  d3 = __extension__(v8df){34, 34, 34, 34, 34, 34, 34, 34};
}
