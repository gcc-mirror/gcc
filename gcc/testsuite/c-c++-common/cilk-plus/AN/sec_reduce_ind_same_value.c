/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

int A[256];

int main () { 
    A[:] = 2; 
    int max_index = 0, min_index = 0;
  
    max_index = __sec_reduce_max_ind (A[:]);
  
    if (max_index != 255)
      return 1;

    min_index = __sec_reduce_min_ind (A[:]);
    if (min_index != 255)
      return 2;

    return 0;
}

