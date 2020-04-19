/* { dg-do compile } */

void foo (void)
{
  int array_of_array[10][10];
  int **ptr_to_ptr;
  int *array_of_ptr[10];
  int (*ptr_to_array)[10];
 
  #pragma acc parallel copy (array_of_array[2:4][0:10])
    array_of_array[5][5] = 1;

  #pragma acc parallel copy (ptr_to_ptr[2:4][1:7])
    ptr_to_ptr[5][5] = 1;

  #pragma acc parallel copy (array_of_ptr[2:4][1:7])
    array_of_ptr[5][5] = 1;

  #pragma acc parallel copy (ptr_to_array[2:4][1:7]) /* { dg-error "array section is not contiguous in 'map' clause" } */
    ptr_to_array[5][5] = 1;
}
/* { dg-final { scan-tree-dump-times {#pragma omp target oacc_parallel map\(tofrom:array_of_array} 1 gimple } } */
/* { dg-final { scan-tree-dump-times {#pragma omp target oacc_parallel map\(tofrom,noncontig_array:ptr_to_ptr \[dimensions: 2 4, 1 7\]} 1 gimple } } */
/* { dg-final { scan-tree-dump-times {#pragma omp target oacc_parallel map\(tofrom,noncontig_array:array_of_ptr \[dimensions: 2 4, 1 7\]} 1 gimple } } */
/* { dg-final { scan-tree-dump-times {#pragma omp target oacc_parallel map\(tofrom,noncontig_array:ptr_to_array \[dimensions: 2 4, 1 7\]} 1 gimple { xfail *-*-* } } } */
