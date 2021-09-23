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

  #pragma omp target map (a to: b) /* { dg-error "'#pragma omp target' with modifier other than 'always' or 'close'" } */
  ;

  #pragma omp target map (close, a to: b) /* { dg-error "'#pragma omp target' with modifier other than 'always' or 'close'" } */
  ;

  #pragma omp target map (close a) /* { dg-error "'close' undeclared" "" { target c } } */ 
  /* { dg-error "'close' has not been declared" "" { target c++ } .-1 } */ 
  /* { dg-error "expected '\\)' before 'a'" "" { target *-*-* } .-2 } */
  ;

  #pragma omp target map (always a) /* { dg-error "'always' undeclared" "" { target c } } */
  /* { dg-error "'always' has not been declared" "" { target c++ } .-1 } */ 
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

  #pragma omp target map (always to : a) map (close to : b)
  ;

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
