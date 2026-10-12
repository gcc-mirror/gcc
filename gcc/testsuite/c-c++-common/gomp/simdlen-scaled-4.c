/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple -w" } */

// PR middle-end/127723

void fvarInt2(int);
void fvarInt0(int);
void fvar0(int);

#pragma omp declare variant(fvarInt2) match(construct={simd(simdlen(scaled(int,2): 8))})
#pragma omp declare variant(fvarInt0) match(construct={simd(simdlen(scaled(int): 8))})
#pragma omp declare variant(fvar0) match(construct={simd(simdlen(4))})
void fbase(int) { }

void f() {
  #pragma omp simd simdlen(scaled(int,2): 8)
  for (int i = 1; i < 0; i++)
    fbase (1); // -> fvarInt2

  #pragma omp simd simdlen(scaled(int): 8)
  for (int i = 1; i < 0; i++)
    fbase (2); // -> fvarInt0

  #pragma omp simd simdlen(4)
  for (int i = 1; i < 0; i++)
    fbase (3); // -> fvar0

  #pragma omp simd simdlen(8)
  for (int i = 1; i < 0; i++)
    fbase (4); // -> fbase
}

// Once implemented remove ...
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 10 }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 11 }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 16 }
// { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 20 }

// ... and the following XFAIL should work ... (it fails because the clause is removed)
// { dg-final { scan-tree-dump-times "fvarInt2 \\(1\\);" 1 "gimple" { xfail *-*-* } } }
// { dg-final { scan-tree-dump-times "fvarInt0 \\(2\\);" 1 "gimple" { xfail *-*-* } } }

// but the following three fail because of PR c++/127734:

// { dg-final { scan-tree-dump-times "fvar0 \\(3\\);" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "fbase \\(4\\);" 1 "gimple" } }


// Same but function-name order reversed:

// { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant base \\(fvar0 match construct = {simd \\(simdlen\\(4\\)\\)}\\), omp declare variant base \\(fvarInt0 match construct = {simd \\(simdlen\\(scaled\\(int\\):8\\)\\)}\\), omp declare variant base \\(fvarInt2 match construct = {simd \\(simdlen\\(scaled\\(int,2\\):8\\)\\)}\\)\\)\\)" "gimple" { target c } } }
// { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant base \\(fvarInt2 match construct = {simd \\(simdlen\\(scaled\\(int,2\\):8\\)\\)}\\), omp declare variant base \\(fvarInt0 match construct = {simd \\(simdlen\\(scaled\\(int\\):8\\)\\)}\\), omp declare variant base \\(fvar0 match construct = {simd \\(simdlen\\(4\\)\\)}\\)\\)\\)" "gimple" { target c++ } } }

