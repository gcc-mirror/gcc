// { dg-do run  }
extern "C" void abort();

template <void* P>
void f(int j);

template <int I>
void f(int j);


template <void* P>
void f(int j)
{
  abort();
}


template <int I>
void f(int j)
{
}


int main()
{
  f<3>(7);
}

