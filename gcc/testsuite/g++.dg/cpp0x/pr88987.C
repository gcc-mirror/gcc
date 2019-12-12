// { dg-do compile { target c++11 } }

int sm;

template <typename T> T
pk () noexcept (sm)  // { dg-error "constant expression" }
{
  return 0;
}
