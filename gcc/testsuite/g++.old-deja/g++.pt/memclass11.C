// Build don't link:

struct S1
{
  template <class T>
  struct S2;

  template <class T>
  struct S2 {
    enum E { a };
  };
};

int i = (int) S1::S2<double>::a;
