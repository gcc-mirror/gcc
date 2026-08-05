// { dg-additional-options "-fdump-tree-original" }

// Check that OpenMP array sections with [: and :] do not get mixed up with C++26's splice specifier,
// used for reflection.
//
// See also OpenMP spec Issue 4740.

void f() {
 char a1[4], a2[4], a3[4], b[5];
 char c1[4], c2[4], c3[4], c4[4];

 #pragma omp target map(to: a1[:], a2[:2], a3[2:], b[1:2], c1[ : ], c2[ : 2], c3[2: ], c4[ :]) 
   ;

  #pragma omp task affinity(a1[:])
    ;
  #pragma omp task affinity(a2[:2])
    ;
  #pragma omp task affinity(a3[2:])
    ;
  #pragma omp task affinity(b[1:2])
    ;
  #pragma omp task affinity(c1[ : ])
    ;
  #pragma omp task affinity(c2[ : 2])
    ;
  #pragma omp task affinity(c3[2: ])
    ;
  #pragma omp task affinity(c4[ :])
    ;

}

// { dg-final { scan-tree-dump "#pragma omp target.* map\\(to:c4\\\[0\\\] \\\[len: 4\\\]\\).* map\\(to:c3\\\[2\\\] \\\[len: 2\\\]\\).* map\\(to:c2\\\[0\\\] \\\[len: 2\\\]\\).* map\\(to:c1\\\[0\\\] \\\[len: 4\\\]\\).* map\\(to:b\\\[1\\\] \\\[len: 2\\\]\\).* map\\(to:a3\\\[2\\\] \\\[len: 2\\\]\\).* map\\(to:a2\\\[0\\\] \\\[len: 2\\\]\\).* map\\(to:a1\\\[0\\\] \\\[len: 4\\\]\\)" "original" } }

// { dg-final { scan-tree-dump "#pragma omp task affinity\\(a1\\\[0\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(a2\\\[0\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(a3\\\[2\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(b\\\[1\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(c1\\\[0\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(c2\\\[0\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(c3\\\[2\\\]\\)" "original" } }
// { dg-final { scan-tree-dump "#pragma omp task affinity\\(c4\\\[0\\\]\\)" "original" } }

