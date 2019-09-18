/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_calls } */

void f(int r1, int *fp) 
{     
  void *hlbl_tbl[] = { &&label1 }; 
  goto *hlbl_tbl[r1]; 
  *fp = 0; 
 label0: 
  fp += 8; 
 label1: 
  *fp = 0; 
  if (r1)  
    goto label2; 
  if (r1) 
    goto label0; 
 label2: 
  ; 
}

int x;
