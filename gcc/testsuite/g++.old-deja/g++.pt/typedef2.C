// Build don't link:

typedef const int cint;

template<class T>
class A
{
public:
  T f(cint i);
};

template <class T>
T A<T>::f(cint i)
{
}

int main()
{
  A<int> a;
}
