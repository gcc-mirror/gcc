int __attribute__((noinline,noclone))
sort(int L)
{
  int end[2] = { 10, 10, }, i=0, R;
  while (i<2)
    {
      R = end[i];
      if (L<R)
        {
          end[i+1] = 1;
          end[i] = 10;
          ++i;
        }
      else
        break;
    }
  return i;
}
extern void abort (void);
int main()
{
  if (sort (5) != 1)
    abort ();
  return 0;
}

