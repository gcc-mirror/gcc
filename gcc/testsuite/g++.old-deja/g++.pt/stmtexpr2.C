extern "C" void abort();

int i;

void g()
{
  i++;
}

template <class T>
void f(T)
{
  __extension__ ({g();});
}

int main()
{
  f(3.0);
  if (i != 1)
    abort();

  return 0;
}
 
