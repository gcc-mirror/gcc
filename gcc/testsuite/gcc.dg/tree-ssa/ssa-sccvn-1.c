/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-fre-stats" } */

void vnum_test8(int *data) 
{ 
  int i; 
  int stop = data[3]; 
  int m = data[4]; 
  int n = m; 
  for (i=0; i<stop; i++) { 
    int k = data[2]; 
    data[k] = 2; 
    data[0] = m - n; 
    k = data[1]; 
    m = m + k; 
    n = n + k; 
  } 
} 
/* We should eliminate m - n, and set n = n + k into n = m. */
/* { dg-final { scan-tree-dump-times "Eliminated: 3" 1 "fre"} } */
/* { dg-final { cleanup-tree-dump "fre" } } */
