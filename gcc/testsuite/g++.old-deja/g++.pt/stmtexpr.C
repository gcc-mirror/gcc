// { dg-do run  }
extern "C" void abort();

template <class T>
T f(T)
{
  T t = __extension__ ({ T j = 4; j + 3; });
  return t;
}


int main()
{
  if (f(3) != 7)
    abort();
}
 
