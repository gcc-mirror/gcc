typedef __SIZE_TYPE__ size_t;

template <class T>
struct A
{
  int size;
  A ()
    {
      T *p;
      p = new T[size];
      int foo;
      foo = 5 * size;
    };
};

struct B
{
  virtual ~B() { }
  void operator delete [] (void *ptr, size_t size) { }
};

int main ()
{
  A<B> *p = new A<B>;
}
