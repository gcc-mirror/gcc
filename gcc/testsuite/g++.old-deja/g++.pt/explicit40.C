extern "C" void abort();

template <void* P>
void f(int j);

template <unsigned int I>
void f(int j);


template <void* P>
void f(int j)
{
  abort();
}


template <unsigned int I>
void f(int j)
{
}


int main()
{
  f<3>(7);
}

