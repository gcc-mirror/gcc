// Build don't run:
// Origin: Andrey Slepuhin <pooh@msu.ru>

namespace A
{
  int j;

  template <typename val_t>
  struct X
  {
    inline X ()
    {
      extern int j;
      i = j;
    }

    int i;
  };
}

int main ()
{
  A::X<int> x;
}
