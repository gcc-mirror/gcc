// { dg-do run  }
extern "C" void abort();

template <class T> 
class A
{
 public:
  static int foo(int);
};

template <>
int A<int>::foo(int i)
{
  return i;
}


int main()
{
  if (A<int>::foo(22) != 22)
    abort();
}
