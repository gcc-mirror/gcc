extern "C" void abort(void);

template <int I>
void f(int i)
{
}

template <void*>
void f(int i) 
{ 
  abort();
}

int main()
{
  f<0>(3);
}
