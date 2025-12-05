/* { dg-do compile }  */
/* { dg-additional-options "-fdump-tree-original" }  */

void f()
{
  int N = 1024;

  #pragma omp target dyn_groupprivate(1024)  // { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    ;

  #pragma omp target dyn_groupprivate (1024 * N)  // { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    ;

  #pragma omp target dyn_groupprivate ( fallback ( abort ) : N)  // { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    ;

  #pragma omp target dyn_groupprivate ( fallback ( null ) : N)  // { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    ;

  #pragma omp target dyn_groupprivate ( fallback ( default_mem ) : N)  // { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    ;
}

/* { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(1024\\)" 1 "original" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(N \\* 1024\\)" 1 "original" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(abort\\):N\\)" 1 "original" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(null\\):N\\)" 1 "original" } }  */
/* { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(default_mem\\):N\\)" 1 "original" } }  */
