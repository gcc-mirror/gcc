// { dg-do run  }
template <class T>
void f(T t, int i = 10);

template <class T>
void f(T t, int i) {}

int main()
{
  f(3);
}
