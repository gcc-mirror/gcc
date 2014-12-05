/* { dg-options "-O2" } */

extern void abort (void);

unsigned int arr[4][4] = {{0, 1, 1, 2}, {2, 2, 1, 2}, {1, 2, 1, 1}, {1, 2, 2, 0}};
unsigned long long
foo ()
{
  unsigned long long ll = 0;
  ll += arr[0][1];
  ll += arr[1][0];
  ll += arr[1][1];
  ll += arr[2][0];
  return ll;
}

/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]" 1 } } */
