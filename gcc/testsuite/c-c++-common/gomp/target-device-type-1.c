// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

void f ()
{

#pragma omp target
  ;

#pragma omp target device_type ( any )
  ;

#pragma omp target device_type ( nohost )  // { dg-message "sorry, unimplemented: only the 'device_type\\(any\\)' is supported" }
  ;

#pragma omp target device_type ( host )
  ;

}

// { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\)\[\\r\\n\]" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(any\\)" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(nohost\\)" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(host\\)" 1 "gimple" } }
