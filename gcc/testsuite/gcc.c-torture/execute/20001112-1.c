int main ()
{
  long long i = 1;

  i = i * 2 + 1;
  
  if (i != 3)
    abort ();
  exit (0);
}
