/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

int sumint(const int arr[]) {
    arr = __builtin_assume_aligned(arr, 64);
    int sum=0;
    for (int i=0 ; i<1024 ; i++)
      sum+=arr[i];
    return sum;
}

/* { dg-final { scan-assembler-times "vextracti" 1 } } */
