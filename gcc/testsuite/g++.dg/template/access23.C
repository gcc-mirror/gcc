template <class T>
class A
{
  typedef T I;
};

template <class T>
void f(typename T::I);

template <class T>
void f(int);

int main()
{
  f<A<float> > (1);
}
