// Build don't link:

template <class T>
class X {
public:
  T x;
};

class Y {
public:
  template <class T> static void f(X<T>& a) {}

  void g(void);
};

void
Y::g(void)
{
  X<int> a;

  f(a);
}

  
