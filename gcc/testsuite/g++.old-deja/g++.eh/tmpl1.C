// { dg-do run  }
template <class T>
void f() throw (T)
{
  throw 7;
}


int main()
{
  try {
    f<int>();
  } catch (...) {
    return 0;
  }
}
