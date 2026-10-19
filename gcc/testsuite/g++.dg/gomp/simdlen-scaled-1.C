/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple -w" } */

// PR middle-end/127723

template<typename T, int d, long l>
[[omp::decl(declare simd,simdlen(scaled(T, d): l))]]
void g1(int a) { }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-2 }

template<typename T, int d, long l>
[[omp::decl(declare simd,simdlen(scaled(T, d + l): sizeof (T)))]]
void g2(int a) { }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-2 }

template<typename T, int d, long l>
[[omp::decl(declare simd,simdlen(scaled(T): l/d))]]
void g3(int a) { }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-2 }

void f() {
  g1<int, 1, 1>(1);
  g2<float, 2, 0>(1);
  g3<_Complex double, 2, 8>(1);
}

/* { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(int,1\\):1\\)\\)\\)\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(float,2\\):4\\)\\)\\)\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(complex double\\):4\\)\\)\\)\\)" "gimple" } } */
