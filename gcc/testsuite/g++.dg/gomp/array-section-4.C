// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fdump-tree-original" }

void f()
{
  constexpr int a = 42;
  constexpr auto b = ^^a;
  int c[128];

  #pragma omp target enter data map(to : c[[:b:]])

  #pragma omp target enter data map(to : c[[:b:]:])

  #pragma omp target enter data map(to : c[:[:b:]])

  #pragma omp target enter data map(to : c[[:b:]:2])

  #pragma omp target enter data map(to : c[2:[:b:]])


  #pragma omp task affinity( c[[:b:]])
    ;

  #pragma omp task affinity( c[[:b:]:2])
    ;
  #pragma omp task affinity( c[2:[:b:]])
    ;

  #pragma omp task affinity( c[[:b:]:])
    ;
  #pragma omp task affinity( c[:[:b:]])
    ;
}

// { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:c\\\[42\\\] \\\[len: 4\\\]\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:c\\\[42\\\] \\\[len: 344\\\]\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:c\\\[0\\\] \\\[len: 168\\\]\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:c\\\[42\\\] \\\[len: 8\\\]\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:c\\\[2\\\] \\\[len: 168\\\]\\)" 1 "original" } }

// { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(c\\\[42\\\]\\)" 3 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(c\\\[2\\\]\\)" 1 "original" } }
// { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(c\\\[0\\\]\\)" 1 "original" } }
