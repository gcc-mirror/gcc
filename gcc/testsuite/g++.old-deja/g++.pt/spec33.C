// Build don't run:
// Origin: James McKelvey <mckelvey@fafnir.com>

class A
{
  public:
  template <class T> A(T x, bool y = false);
};

template <class T> A::A(T, bool)
{
}

template <> A::A(char, bool)
{
}

int main()
{
  int  b;
  char c;

  A x(b);
  A y(c);
  A z(c, false);
}
