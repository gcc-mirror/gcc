template<typename T>
struct A
{
  typedef T type();
  type i : 2;  // { dg-error "8:cannot declare bit-field" }
};
