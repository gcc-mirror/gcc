/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

void
foo (void)
{
  /* Test to ensure that the close modifier is parsed and ignored in map clauses. */
  int a, b, b1, b2, b3, b4, b5, b6, b7;

  #pragma omp target map (a)
  ;

  #pragma omp target map (to:a)
  ;

  #pragma omp target map (a to: b) /* { dg-error "'map' clause with map-type modifier other than 'always', 'close' or 'present'" } */
  ;

  #pragma omp target map (close, a to: b) /* { dg-error "'map' clause with map-type modifier other than 'always', 'close' or 'present'" } */
  ;

  #pragma omp target enter data map(b7) map (close, a to: b) /* { dg-error "'map' clause with map-type modifier other than 'always', 'close' or 'present'" } */
  ;

  #pragma omp target exit data map(b7) map (close, a from: b) /* { dg-error "'map' clause with map-type modifier other than 'always', 'close' or 'present'" } */
  ;

  #pragma omp target data map(b7) map (close, a from: b) /* { dg-error "'map' clause with map-type modifier other than 'always', 'close' or 'present'" } */
  ;


  #pragma omp target map (close a) /* { dg-error "'close' undeclared" "" { target c } } */ 
  /* { dg-error "'close' was not declared in this scope" "" { target c++ } .-1 } */ 
  /* { dg-error "expected '\\)' before 'a'" "" { target *-*-* } .-2 } */
  ;

  #pragma omp target map (always a) /* { dg-error "'always' undeclared" "" { target c } } */
  /* { dg-error "'always' was not declared in this scope" "" { target c++ } .-1 } */ 
  /* { dg-error "expected '\\)' before 'a'" "" { target *-*-* } .-2 } */
  ;

  #pragma omp target map (close to:a)
  ;

  #pragma omp target map (close, to:a)
  ;

  #pragma omp target map (close delete:a) /* { dg-error "'#pragma omp target' with map-type other than 'to', 'from', 'tofrom' or 'alloc' on 'map' clause" } */
  ;

  #pragma omp target map (close always to:b1)
  ;

  #pragma omp target map (close, always to:b2)
  ;

  #pragma omp target map (close, always, to:b3)
  ;

  #pragma omp target map (always close to:b4)
  ;

  #pragma omp target map (always, close to:b5)
  ;

  #pragma omp target map (always, close, to:b6)
  ;

  #pragma omp target map (always, always, to:a) /* { dg-error "too many 'always' modifiers" } */
  ;

  #pragma omp target map (always always, to:a) /* { dg-error "too many 'always' modifiers" } */
  ;

  #pragma omp target map (always, always to:a) /* { dg-error "too many 'always' modifiers" } */
  ;

  #pragma omp target map (always always to:a) /* { dg-error "too many 'always' modifiers" } */
  ;

  #pragma omp target map (close, close, to:a) /* { dg-error "too many 'close' modifiers" } */
  ;

  #pragma omp target map (close close, to:a) /* { dg-error "too many 'close' modifiers" } */
  ;

  #pragma omp target map (close, close to:a) /* { dg-error "too many 'close' modifiers" } */
  ;

  #pragma omp target map (close close to:a) /* { dg-error "too many 'close' modifiers" } */
  ;

  #pragma omp target map (present , present , to:a) /* { dg-error "too many 'present' modifiers" } */
  ;

  #pragma omp target map (present  present , to:a) /* { dg-error "too many 'present' modifiers" } */
  ;

  #pragma omp target map (present , present  to:a) /* { dg-error "too many 'present' modifiers" } */
  ;

  #pragma omp target map (present  present  to:a) /* { dg-error "too many 'present' modifiers" } */
  ;

  #pragma omp target map (always to : a) map (close to : b) map (present,tofrom : b1) map(always,present,alloc : b2) map(close, from: b3)
  ;

  #pragma omp target data map(tofrom:b1)
  ;
  #pragma omp target data map(close,tofrom:b1)
  ;
  #pragma omp target data map(close always,tofrom:b1)
  ;
  #pragma omp target data map(close always,tofrom:b1)
  ;
  #pragma omp target data map(close present,tofrom:b1)
  ;
  #pragma omp target data map(close present,tofrom:b1)
  ;
  #pragma omp target data map(always close present,tofrom:b1)
  ;
  #pragma omp target data map(always close present,tofrom:b1)
  ;

  #pragma omp target enter data map(alloc: a) map(to:b) map(tofrom:b1)
  #pragma omp target enter data map(close, alloc: a) map(close,to:b) map(close,tofrom:b1)
  #pragma omp target enter data map(always,alloc: a) map(always,to:b) map(close always,tofrom:b1)
  #pragma omp target enter data map(always,close,alloc: a) map(close,always,to:b) map(close always,tofrom:b1)
  #pragma omp target enter data map(present,alloc: a) map(present,to:b) map(close present,tofrom:b1)
  #pragma omp target enter data map(present,close,alloc: a) map(close,present,to:b) map(close present,tofrom:b1)
  #pragma omp target enter data map(present,always,alloc: a) map(always,present,to:b) map(always close present,tofrom:b1)
  #pragma omp target enter data map(present,always,close,alloc: a) map(close,present,always,to:b) map(always close present,tofrom:b1)

  #pragma omp target exit data map(delete: a) map(release:b) map(from:b1)
  #pragma omp target exit data map(close,delete: a) map(close,release:b) map(close,from:b1)
  #pragma omp target exit data map(always,delete: a) map(always,release:b) map(close always,from:b1)
  #pragma omp target exit data map(always,close,delete: a) map(close,always,release:b) map(close always,from:b1)
  #pragma omp target exit data map(present,delete: a) map(present,release:b) map(close present,from:b1)
  #pragma omp target exit data map(present,close,delete: a) map(close,present,release:b) map(close present,from:b1)
  #pragma omp target exit data map(present,always,delete: a) map(always,present,release:b) map(always close present,from:b1)
  #pragma omp target exit data map(present,always,close,delete: a) map(close,present,always,release:b) map(always close present,from:b1)

  int close = 0;
  #pragma omp target map (close) 
  ;

  #pragma omp target map (close a) /* { dg-error "expected '\\)' before 'a'" } */ 
  ;

  int always = 0;
  #pragma omp target map (always)
  ;

  #pragma omp target map (always a) /* { dg-error "expected '\\)' before 'a'" } */
  ;

  #pragma omp target map (always, close)
  ;

  #pragma omp target map (always, always)  /* { dg-error "'always' appears more than once in map clauses" } */
  ;

  #pragma omp target map (always, always, close)  /* { dg-error "'always' appears more than once in map clauses" } */
  ;

  #pragma omp target map (always, close, to: always, close, b7)
  ;

  int to = 0;
  #pragma omp target map (always, close, to)
  ;

  #pragma omp target map (to, always, close)
    {
      to = always = close = 1;
    }
  if (to != 1 || always != 1 || close != 1)
    __builtin_abort ();
  ;
}

/* { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } } */

/* { dg-final { scan-tree-dump-times "pragma omp target map\\(always,to:" 7 "original" } } */

/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b1" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b2" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b3" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b4" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b5" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b6" "original" } } */
/* { dg-final { scan-tree-dump "pragma omp target map\\(always,to:b7\\) map\\(always,to:close\\) map\\(always,to:always\\)" "original" } } */

/* Note: 'always,alloc' is the same as 'alloc'; hence, there is no 'always' for 'b'. Additionally, 'close' is ignored. */

/* { dg-final { scan-tree-dump "#pragma omp target map\\(from:b3\\) map\\(present,alloc:b2\\) map\\(present,tofrom:b1\\) map\\(to:b\\) map\\(always,to:a\\)" "original" } } */

/* { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:b1\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,tofrom:b1\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target data map\\(present,tofrom:b1\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,present,tofrom:b1\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:b1\\) map\\(to:b\\) map\\(alloc:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(always,to:b1\\) map\\(always,to:b\\) map\\(alloc:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(present,to:b1\\) map\\(present,to:b\\) map\\(present,alloc:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(always,present,to:b1\\) map\\(always,present,to:b\\) map\\(present,alloc:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(from:b1\\) map\\(release:b\\) map\\(delete:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(always,from:b1\\) map\\(release:b\\) map\\(delete:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(present,from:b1\\) map\\(release:b\\) map\\(delete:a\\)\[\r\n\]" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(always,present,from:b1\\) map\\(release:b\\) map\\(delete:a\\)\[\r\n\]" 2 "original" } } */
