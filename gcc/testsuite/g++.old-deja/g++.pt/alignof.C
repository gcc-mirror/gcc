// { dg-do run  }
extern "C" void abort();

struct S
{
  char c;
  double d;
};


template <class T>
void foo(T)
{
  if (__alignof__(T) != __alignof__(S))
    abort();
}


int main()
{
  foo(S());
}
