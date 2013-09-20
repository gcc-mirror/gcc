/* { dg-do run } */

long long arr[6] = {0, 1, 2, 3, 4, 5};
extern  void abort (void);
void __attribute__((noinline,noclone))
foo (long long sum)
{
  asm ("");
}
int main()
{
  int i, n = 5;
  long long sum = 0, prevsum = 0;

  for(i = 1; i <= n; i++)
    {
      foo (sum);
      sum = (i - 1) * arr[i] - prevsum;
      prevsum += arr[i];
    }

  if (sum != 10)
    abort ();
  return 0;
}
