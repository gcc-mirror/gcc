extern void abort (void);

int arr[] = {1,2,3,4};
int count = 0;

int __attribute__((noinline))
incr (void)
{
  return ++count;
}

int main()
{
  arr[count++] = incr ();
  if (count != 2 || arr[count] != 3)
    abort ();
  return 0;
}
