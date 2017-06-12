// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

template < typename T > 
int foo (int x, T t)
{
  return __transaction_atomic noexcept (foo) (1);  // { dg-error "cannot resolve" }
}
