extern "C" void abort ();

template <class T> int f ()
{
  return sizeof(T);
}

int main ()
{
  if (f<long> () != sizeof(long)
      || f<char> () != sizeof(char)
      || f<long> () != sizeof(long)
      || f<long int> () != sizeof(long int))
    abort ();
}
