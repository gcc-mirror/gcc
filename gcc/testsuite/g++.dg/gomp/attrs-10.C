// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp -ffat-lto-objects -fdump-tree-gimple" }

extern "C" void abort ();

[[omp::directive (declare simd, linear (l))]] extern int f1 (int l);
extern int f2 (int), f3 [[omp::directive (declare simd, uniform (m))]] (int m), f4 (int), z;
#ifdef __aarch64__
[[omp::directive (declare simd, linear (l), simdlen(4))]] extern int f5 [[omp::directive (declare simd uniform (l) simdlen (2) notinbranch)]] (int l);
#else
[[omp::directive (declare simd, linear (l), simdlen(4))]] extern int f5 [[omp::directive (declare simd uniform (l) simdlen (8) notinbranch)]] (int l);
#endif

int
f1 (int l)
{
  return l;
}

// { dg-final { scan-assembler-times "_ZGVnN2l__Z2f1i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2l__Z2f1i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN4l__Z2f1i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4l__Z2f1i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16l__Z2f1i:" 1 { target { i?86-*-* x86_64-*-* } } } }

int
f2 (int l)
{
  return l + 1;
}

// { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]__Z2f2i:" { target { i?86-*-* x86_64-*-* aarch64*-*-* } } } }

int
f3 (int l)
{
  return l + 2;
}

// { dg-final { scan-assembler-times "_ZGVnN2u__Z2f3i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2u__Z2f3i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN4u__Z2f3i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4u__Z2f3i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16u__Z2f3i:" 1 { target { i?86-*-* x86_64-*-* } } } }

int
f4 (int l)
{
  return l + 3;
}

// { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]__Z2f4i:" { target { i?86-*-* x86_64-*-* aarch64*-*-* } } } }

int
f5 (int l)
{
  return l + 4;
}

// { dg-final { scan-assembler-times "_ZGVnN4l__Z2f5i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4l__Z2f5i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN2u__Z2f5i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN4l__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN8u__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8u__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8u__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8u__Z2f5i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGV\[bcde]M8u__Z2f5i:" { target { i?86-*-* x86_64-*-* } } } }

[[omp::directive (declare simd, linear (l), simdlen(4), notinbranch),
  omp::directive (declare simd, uniform (l), simdlen(4), inbranch)]]
int
#ifdef __aarch64__
f6 [[omp::sequence (directive (declare simd uniform (l) simdlen (2), notinbranch),
		    omp::directive (declare simd linear (l) simdlen (2) inbranch))]] (int l)
#else
f6 [[omp::sequence (directive (declare simd uniform (l) simdlen (8), notinbranch),
		    omp::directive (declare simd linear (l) simdlen (8) inbranch))]] (int l)
#endif
{
  return l + 5;
}

// { dg-final { scan-assembler-times "_ZGVnN4l__Z2f6i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4u__Z2f6i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN2u__Z2f6i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbM8l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN8u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM8l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM4u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN4l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM4u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN4l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM8l__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8u__Z2f6i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGV\[bcde]M4l__Z2f6i:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGV\[bcde]N4u__Z2f6i:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGV\[bcde]M8u__Z2f6i:" { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-not "_ZGV\[bcde]N8l__Z2f6i:" { target { i?86-*-* x86_64-*-* } } } }

int
f7 (int l)
{
  return l + 6;
}

// { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]__Z2f7i:" { target { i?86-*-* x86_64-*-* aarch64*-*-* } } } }

int
f8 (int l)
{
  return l + 7;
}

// { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]__Z2f8i:" { target { i?86-*-* x86_64-*-* aarch64*-*-* } } } }

[[omp::sequence (omp::directive (declare variant (f7), match (construct={parallel})),
		 directive (declare simd uniform (l), simdlen(4)))]]
int
#ifdef __aarch64__
f9 [[omp::directive (declare simd uniform (l) simdlen (2)),
#else
f9 [[omp::directive (declare simd uniform (l) simdlen (8)),
#endif
     omp::directive (declare variant (f8) match (construct={parallel,for}))]] (int l)
{
  return l + 8;
}

// { dg-final { scan-assembler-times "_ZGVnN2u__Z2f9i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2u__Z2f9i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN4u__Z2f9i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4u__Z2f9i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN4u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbM8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8u__Z2f9i:" 1 { target { i?86-*-* x86_64-*-* } } } }

int z;

void
test ()
{
  [[omp::directive (parallel)]]
  if (f9 (3) != 9)
    abort ();
  [[omp::directive (parallel for)]]
  for (int i = 0; i < 1; i++)
    if (f9 (4) != 11)
      abort ();
  if (f9 (5) != 13)
    abort ();
}

// { dg-final { scan-tree-dump-times " = f7 \\\(3\\\);" 1 "gimple" } }
// { dg-final { scan-tree-dump-times " = f8 \\\(4\\\);" 1 "gimple" } }
// { dg-final { scan-tree-dump-times " = f9 \\\(5\\\);" 1 "gimple" } }

template <int N>
int
f10 (int x)
{
  return x + N;
}

template [[omp::directive (declare simd, notinbranch)]] int f10<0> (int);

// { dg-final { scan-assembler-times "_ZGVnN2v__Z3f10ILi0EEii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN4v__Z3f10ILi0EEii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbN4v__Z3f10ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4v__Z3f10ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8v__Z3f10ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16v__Z3f10ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }

template  int f10<1> [[omp::directive (declare simd inbranch linear(x))]] (int x);

// { dg-final { scan-assembler-times "_ZGVnM2l__Z3f10ILi1EEii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4l__Z3f10ILi1EEii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4l__Z3f10ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4l__Z3f10ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8l__Z3f10ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16l__Z3f10ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }

template <int N>
int f11 (int);

template <> [[omp::directive (declare simd, inbranch)]] int
f11<0> (int x)
{
  return x;
}

// { dg-final { scan-assembler-times "_ZGVnM2v__Z3f11ILi0EEii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4v__Z3f11ILi0EEii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4v__Z3f11ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4v__Z3f11ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8v__Z3f11ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16v__Z3f11ILi0EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }

template <> int
f11<1> [[omp::directive (declare simd, notinbranch, linear (y))]] (int y)
{
  return y;
}

// { dg-final { scan-assembler-times "_ZGVnN2l__Z3f11ILi1EEii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN4l__Z3f11ILi1EEii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbN4l__Z3f11ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4l__Z3f11ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8l__Z3f11ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16l__Z3f11ILi1EEii:" 1 { target { i?86-*-* x86_64-*-* } } } }

struct S
{
  [[omp::sequence (directive (declare simd, inbranch, uniform (this)))]] int f12 (int x);
  int f13 [[gnu::noinline, omp::directive (declare simd notinbranch uniform (this) linear (y))]] (int y) { return y; }
};

int
S::f12 (int x)
{
  return x;
}

// { dg-final { scan-assembler-times "_ZGVnM2uv__ZN1S3f12Ei:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2uv__ZN1S3f12Ei:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4uv__ZN1S3f12Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4uv__ZN1S3f12Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8uv__ZN1S3f12Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16uv__ZN1S3f12Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbN4ul__ZN1S3f13Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4ul__ZN1S3f13Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8ul__ZN1S3f13Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16ul__ZN1S3f13Ei:" 1 { target { i?86-*-* x86_64-*-* } } } }

int
f14 (S &p, int x)
{
  return p.f13 (x);
}
