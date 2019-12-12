/* Verify that OpenACC 'declare' cleans up for VLAs.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 (void)
{
#define N_f1 1000
  int A_f1[N_f1];
#pragma acc declare copy(A_f1)
  /* { dg-final { scan-tree-dump-times {#pragma omp target oacc_declare map\(to:A_f1} 1 gimple } }
     { dg-final { scan-tree-dump-times {#pragma omp target oacc_declare map\(from:A_f1} 1 gimple } } */
}

void f2 (void)
{
  int N_f2 = 1000;
  int A_f2[N_f2];
#pragma acc declare copy(A_f2)
  /* { dg-final { scan-tree-dump-times {#pragma omp target oacc_declare map\(to:\(\*A_f2} 1 gimple } }
     { dg-final { scan-tree-dump-times {#pragma omp target oacc_declare map\(from:\(\*A_f2} 1 gimple { xfail *-*-* } } } TODO PR90861 */
}
