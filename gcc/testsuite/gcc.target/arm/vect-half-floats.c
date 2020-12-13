/* { dg-do compile }  */
/* { dg-require-effective-target target_float16 } */ 
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-add-options for_float16 } */
/* { dg-additional-options "-Ofast -ftree-vectorize -fdump-tree-vect-all -std=c11" } */

void foo (_Float16 n1[], _Float16 n2[], _Float16 r[], int n)
{
  for (int i = 0; i < n; i++)
   r[i] = n1[i] + n2[i];
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

