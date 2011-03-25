/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-fre1-stats" } */

int vnum_test8(int *data) 
{ 
  int i; 
  int stop = data[3]; 
  int m = data[4]; 
  int n = m; 
  int p = 0;

  for (i=0; i<stop; i++) { 
    int k = data[2]; 
    data[5] = 0;
    if (i < 30)
      data[5] = m - n;
    p = data[5];
    k = data[1]; 
    m = m + k; 
    n = n + k; 
  } 
  return p;
} 
/* We should eliminate m - n, n + k, set data[5] = 0, eliminate the
   address arithmetic for data[5], and set p = 0.
/* { dg-final { scan-tree-dump-times "Eliminated: 5" 1 "fre1"} } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
