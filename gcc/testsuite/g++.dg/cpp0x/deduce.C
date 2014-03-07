// { dg-do compile { target c++11 } }
template<typename T, typename U> struct same_type;
template<typename T> struct same_type<T, T> {};

int lval_int;
int rval_int();
int const lval_const_int=0;
int const&& rval_const_int();

template <typename T> void deduce_lval_int(T && t)
{
  same_type<T, int &>();
}

template <typename T> void deduce_rval_int(T && t)
{
  same_type<T, int>();
}

template <typename T> void deduce_lval_const_int(T && t)
{
  same_type<T, const int &>();
}

template <typename T> void deduce_rval_const_int(T && t)
{
  same_type<T, const int>();
}

void f()
{
  deduce_lval_int(lval_int);
  deduce_rval_int(rval_int());
  deduce_lval_const_int(lval_const_int);
  deduce_rval_const_int(rval_const_int());
}
