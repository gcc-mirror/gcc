// { dg-additional-options "-fdump-tree-gimple" }

// Remove the comments from the reduction test
// after the FE learns that reduction variables may appear in data clauses too.

void
test ()
{
  int a[100], i, j, z;


  // acc parallel

  #pragma acc parallel loop collapse (2)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop gang
  for (i = 0; i < 100; i++)
    ;

  #pragma acc parallel loop worker
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop vector
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop seq
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop auto
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop tile (2, 3)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc parallel loop independent
  for (i = 0; i < 100; i++)
    ;

  #pragma acc parallel loop private (z)
  for (i = 0; i < 100; i++)
    z = 0;

//  #pragma acc parallel loop reduction (+:z) copy (z)
//  for (i = 0; i < 100; i++)
//    ;


  // acc kernels

  #pragma acc kernels loop collapse (2)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop gang
  for (i = 0; i < 100; i++)
    ;

  #pragma acc kernels loop worker
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop vector
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop seq
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop auto
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop tile (2, 3)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc kernels loop independent
  for (i = 0; i < 100; i++)
    ;

  #pragma acc kernels loop private (z)
  for (i = 0; i < 100; i++)
    z = 0;

//  #pragma acc kernels loop reduction (+:z) copy (z)
//  for (i = 0; i < 100; i++)
//    ;

  
  // acc serial

  #pragma acc serial loop collapse (2)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop gang
  for (i = 0; i < 100; i++)
    ;

  #pragma acc serial loop worker
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop vector
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop seq
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop auto
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop tile (2, 3)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 10; j++)
      ;

  #pragma acc serial loop independent
  for (i = 0; i < 100; i++)
    ;

  #pragma acc serial loop private (z)
  for (i = 0; i < 100; i++)
    z = 0;

//  #pragma acc serial loop reduction (+:z) copy (z)
//  for (i = 0; i < 100; i++)
//    ;
}


// { dg-final { scan-tree-dump-times "acc loop collapse.2. private.j. private.i" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop gang" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop worker" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop vector" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop seq" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop auto" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop tile.2, 3" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "acc loop independent private.i" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "private.z" 3 "gimple" } }
