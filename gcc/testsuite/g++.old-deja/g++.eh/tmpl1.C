// { dg-do run  }
template <class T>
void f()
#if __cplusplus <= 201402L
throw (T)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
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
