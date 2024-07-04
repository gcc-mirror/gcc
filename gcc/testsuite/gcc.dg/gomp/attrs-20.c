/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23 -ffat-lto-objects -fdump-tree-gimple" } */

extern void abort ();

[[omp::decl (declare simd, linear (l))]] extern int f1 (int l);
extern int f2 (int), f3 [[omp::decl (declare simd, uniform (m))]] (int m), f4 (int), z;
[[omp::decl (declare simd, linear (l), simdlen(4))]] extern int f5 [[omp::decl (declare simd uniform (l) simdlen (8) notinbranch)]] (int l);

int
f1 (int l)
{
  return l;
}

/* { dg-final { scan-assembler-times "_ZGVbM4l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16l_f1:" 1 { target { i?86-*-* x86_64-*-* } } } } */

int
f2 (int l)
{
  return l + 1;
}

/* { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]_f2:" { target { i?86-*-* x86_64-*-* } } } } */

int
f3 (int l)
{
  return l + 2;
}

/* { dg-final { scan-assembler-times "_ZGVbM4u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16u_f3:" 1 { target { i?86-*-* x86_64-*-* } } } } */

int
f4 (int l)
{
  return l + 3;
}

/* { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]_f4:" { target { i?86-*-* x86_64-*-* } } } } */

int
f5 (int l)
{	/* { dg-warning "GCC does not currently support simdlen 8 for type 'int'" "" { target aarch64*-*-* } .-1 } */
  return l + 4;
}

/* { dg-final { scan-assembler-times "_ZGVbM4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8u_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8u_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8u_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8u_f5:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-not "_ZGV\[bcde]M8u_f5:" { target { i?86-*-* x86_64-*-* } } } } */

[[omp::decl (declare simd, linear (l), simdlen(4), notinbranch),
  omp::decl (declare simd, uniform (l), simdlen(4), inbranch)]]
int
f6 [[omp::decl (declare simd uniform (l) simdlen (8), notinbranch),
     omp::decl (declare simd linear (l) simdlen (8) inbranch)]] (int l)
{	/* { dg-warning "GCC does not currently support simdlen 8 for type 'int'" "" { target aarch64*-*-* } .-2 } */
  return l + 5;
}

/* { dg-final { scan-assembler-times "_ZGVbM4u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM8l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8l_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8u_f6:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-not "_ZGV\[bcde]M4l_f6:" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-not "_ZGV\[bcde]N4u_f6:" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-not "_ZGV\[bcde]M8u_f6:" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-not "_ZGV\[bcde]N8l_f6:" { target { i?86-*-* x86_64-*-* } } } } */

int
f7 (int l)
{
  return l + 6;
}

/* { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]_f7:" { target { i?86-*-* x86_64-*-* } } } } */

int
f8 (int l)
{
  return l + 7;
}

/* { dg-final { scan-assembler-not "_ZGV\[a-zA-Z0-9]_f8:" { target { i?86-*-* x86_64-*-* } } } } */

[[omp::decl (declare variant (f7), match (construct={parallel})),
  omp::decl (declare simd uniform (l), simdlen(4))]]
int
f9 [[omp::decl (declare simd uniform (l) simdlen (8)),
     omp::decl (declare variant (f8) match (construct={parallel,for}))]] (int l)
{	/* { dg-warning "GCC does not currently support simdlen 8 for type 'int'" "" { target aarch64*-*-* } .-2 } */
  return l + 8;
}

/* { dg-final { scan-assembler-times "_ZGVbM4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8u_f9:" 1 { target { i?86-*-* x86_64-*-* } } } } */

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

/* { dg-final { scan-tree-dump-times " = f7 \\\(3\\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times " = f8 \\\(4\\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times " = f9 \\\(5\\\);" 1 "gimple" } } */

int
f10 (int x)
{
  return x;
}

[[omp::decl (declare simd, notinbranch)]] int f10 (int);

/* { dg-final { scan-assembler-times "_ZGVbN4v_f10:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4v_f10:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8v_f10:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16v_f10:" 1 { target { i?86-*-* x86_64-*-* } } } } */

int
f11 (int x)
{
  return x + 1;
}

int f11 [[omp::decl (declare simd inbranch linear(x))]] (int x);

/* { dg-final { scan-assembler-times "_ZGVbM4l_f11:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l_f11:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8l_f11:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16l_f11:" 1 { target { i?86-*-* x86_64-*-* } } } } */
