int main ()
{
  int v = 42;

  static inline int fff (int x)
    {
      return x*10;
    }

  return (fff (v) != 420);
}
