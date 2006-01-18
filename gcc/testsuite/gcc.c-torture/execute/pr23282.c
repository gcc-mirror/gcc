void abort (void);

int main (void)
{
  int j, a, b;
      
  for (j = 0; j < 2; j++)
    {
      a = j * j;
      b = a - 2 * j;
    }
  if (b != -1)
    abort ();
  return 0;
}
