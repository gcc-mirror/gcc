int main ()
{
  int v = 42;

  inline int fff (int x)
    {
      return x*10;
    }

  return (fff (v) != 420);
}
