/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts" } */

void vnum_test8(int *data) 
{ 
  int i; 
  int stop = data[3]; 
  int m = data[4]; 
  int n = m; 
  for (i=1; i<stop; i++) { 
    int k = data[2]; 
    data[k] = 2; 
    data[0] = m - n; 
    k = data[1]; 
    m = m + k; 
    n = n + k; 
  } 
} 

/* Using the SCEV analysis, this loop should be transformed to:

   | void vnum_result8(int *data) 
   |{ 
   |  int i; 
   |  int stop = data[3]; 
   |  for (i=1; i<stop; i++) { 
   |    int k = data[2]; 
   |    data[k] = 2; 
   |    data[0] = 0; 
   |  } 
   |}

*/

/* { dg-final { scan-tree-dump-times "= 0;" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "= 2;" 1 "ivopts"} } */
