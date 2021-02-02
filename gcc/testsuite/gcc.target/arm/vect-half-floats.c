/* { dg-do compile }  */
/* { dg-options "-Ofast -ftree-vectorize -fdump-tree-vect-all -std=c11" } */
/* { dg-require-effective-target float16 } */ 
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-add-options float16 } */

void foo (_Float16 n1[], _Float16 n2[], _Float16 r[], int n)
{
  for (int i = 0; i < n; i++)
   r[i] = n1[i] + n2[i];
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

