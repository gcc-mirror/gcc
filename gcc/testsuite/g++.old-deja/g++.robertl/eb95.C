// { dg-do run  }
struct A {};

template <class T>
void operator+ (A &i, T &b) {}

template<class T>
void func (A &a, T &b) {}

int main()
{
  A a;

#if STRANGE
  func(a, "egcs");
#endif
  a+"egcs";
}
