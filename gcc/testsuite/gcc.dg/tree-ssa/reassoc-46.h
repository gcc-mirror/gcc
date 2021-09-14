#define M 1024
unsigned int arr1[M];
unsigned int arr2[M];
volatile unsigned int sink;

unsigned int
test (void)
{
  unsigned int sum = 0;
  for (int i = 0; i < M; i++)
    {
#ifdef MODIFY
      /* Modify the loop accumulator using a chain of operations - this should
         not affect its rank biasing.  */
      sum |= 1;
      sum ^= 2;
#endif
#ifdef STORE
      /* Save the loop accumulator into a global variable - this should not
         affect its rank biasing.  */
      sink = sum;
#endif
#ifdef USE
      /* Add a tricky use of the loop accumulator - this should prevent its
         rank biasing.  */
      i = (i + sum) % M;
#endif
      /* Use addends with different ranks.  */
      sum += arr1[i];
      sum += arr2[((i ^ 1) + 1) % M];
    }
  return sum;
}
