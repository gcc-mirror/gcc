// { dg-do assemble  }
// Origin: Jean-Francois Panisset <panisset@discreet.com>

template<class T>
void foo(T *data)
{
  ((char *)data)->~T();
}
