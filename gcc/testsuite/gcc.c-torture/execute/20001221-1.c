int main ()
{
  unsigned long long a;
  if (! (a = 0xfedcba9876543210ULL))
    abort ();
  exit (0);
}
