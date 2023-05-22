int digit_sum (unsigned long n)
{
  int sum = 0;

  do
    {
      int x = n % 10;
      n /= 10;
      sum += x;
    } while(n);

  return sum;
}
