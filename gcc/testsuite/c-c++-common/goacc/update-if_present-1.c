/* Test valid usages of the if_present clause.  */

/* { dg-additional-options "-fdump-tree-omplower" } */

void
t ()
{
  int a, b, c[10];

#pragma acc update self(a) if_present
#pragma acc update device(b) async if_present
#pragma acc update host(c[1:3]) wait(4) if_present
#pragma acc update self(c) device(b) host (a) async(10) if (a == 5) if_present

#pragma acc update self(a)
#pragma acc update device(b) async
#pragma acc update host(c[1:3]) wait(4)
#pragma acc update self(c) device(b) host (a) async(10) if (a == 5)
}

/* { dg-final { scan-tree-dump-times "omp target oacc_update if_present map.from:a .len: 4.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update if_present async.-1. map.to:b .len: 4.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update if_present wait.4. map.from:c.1. .len: 12.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update if_present if.... async.10. map.from:a .len: 4.. map.to:b .len: 4.. map.from:c .len: 40.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update map.force_from:a .len: 4.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update async.-1. map.force_to:b .len: 4.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update wait.4. map.force_from:c.1. .len: 12.." 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_update if.... async.10. map.force_from:a .len: 4.. map.force_to:b .len: 4.. map.force_from:c .len: 40.." 1 "omplower" } } */
