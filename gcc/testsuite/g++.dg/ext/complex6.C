// PR c++/46304
// { dg-options "" }

template<class T>
void f()
{
  __complex double d =  1.0 + 2.0i;
}

template void f<int>();
