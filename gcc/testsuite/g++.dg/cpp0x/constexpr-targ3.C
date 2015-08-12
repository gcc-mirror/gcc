// PR c++/67108
// { dg-do compile { target c++11 } }

template < typename, typename > struct is_same;
template < typename T > struct is_same <T, T >
{
  enum
  {
    value = true
  }
   ;
    constexpr bool operator () ()
  {
    return value;
  }
}
 ;
template < bool, typename = void >struct enable_if;
template < typename T > struct enable_if <true, T >
{
  typedef T type;
}
 ;
struct A;
template < typename, typename = void >struct F;
template < typename X > struct F <X, typename enable_if < is_same < X, A >
{
}
  () >::type >
{
  template < typename MakeDependent > F (MakeDependent)
  {
  }
}
;

int main ()
{
  F < A > (1);
}
