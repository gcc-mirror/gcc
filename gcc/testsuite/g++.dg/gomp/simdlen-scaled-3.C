/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple -w" } */

// PR c++/127734
// PR middle-end/127723

template<typename T>
void f(T);

#pragma omp declare variant(f) match(construct={simd(simdlen(scaled(T, 1): 4))})  // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
template<typename T>
void f0(T) { }

void g() {
  f0<int>(1);
}

#pragma omp declare simd simdlen(scaled(T, 1): 4)  // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
template<typename T>
void h() { }

void c() {
  h<float>();
}

// { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant base \\(f match construct = {simd \\(simdlen\\(scaled\\(int,1\\):4\\)\\)}\\)\\)\\)" "gimple" } }
// { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(float,1\\):4\\)\\)\\)\\)" "gimple" } }
