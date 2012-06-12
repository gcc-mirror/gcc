// PR c++/53599

template <typename T>
int foo ()
{
  struct F;
  struct G
  {
    static int F::* bar();
  };

  return sizeof(G);
}

int z = foo <int> ();
