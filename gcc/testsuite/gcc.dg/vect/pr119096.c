#include "tree-vect.h"

long __attribute__((noipa))
sum(int* A, int* B)
{
        long total = 0;
        for(int j = 0; j < 16; j++)
                if((A[j] > 0) & (B[j] > 0))
                        total += (long)A[j];
        return total;
}
int main()
{
  int A[16] = { 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1 };
  int B[16] = { };
  check_vect ();
  if (sum (A, B) != 0)
    abort ();
  return 0;
}

