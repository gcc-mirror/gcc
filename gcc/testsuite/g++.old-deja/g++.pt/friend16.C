// Build don't link:

template <class T>
class S2
{
public:
  static void f();
};


template <class U>
class S1
{
  template <class T>
  friend class S2;

  static int i;
};


template <class T>
void S2<T>::f() 
{
  S1<T>::i = 3;
}

void g()
{
  S2<double>::f();
  S2<char>::f();
}
