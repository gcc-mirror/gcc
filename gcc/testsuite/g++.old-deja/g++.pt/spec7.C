extern "C" void abort();

template <class T>
void f(T t1, T t2);

template <>
void f(int i, int j) 
{
  abort();
}

void f(short s, char c)
{
}

int main()
{
  f(3, 'c');
}
