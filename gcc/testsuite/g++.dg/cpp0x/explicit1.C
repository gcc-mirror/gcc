// Test for explicit conversion ops from N2437.
// { dg-do compile { target c++11 } }

class U; class V;
class T
{
public:
  T( U const & );
  //implicit converting ctor
  explicit T( V const & );
  // explicit ctor
};
class U
{
};
class V
{
};
class W
{
public:
  operator T() const;
};
class X
{
public:
  explicit operator T() const; // theoretical
};
int main()
{
  U u; V v; W w; X x;
  // Direct initialization:
  T t1( u );
  T t2( v );
  T t3( w );
  T t4( x );
  // Copy initialization:
  T t5 = u;
  T t6 = v;			// { dg-error "" }
  T t7 = w;
  T t8 = x;			// { dg-error "" }
  // Cast notation:
  T t9 = (T) u;
  T t10 = (T) v;
  T t11 = (T) w;
  T t12 = (T) x;
  // Static cast:
  T t13 = static_cast<T>( u );
  T t14 = static_cast<T>( v );
  T t15 = static_cast<T>( w );
  T t16 = static_cast<T>( x );
  // Function-style cast:
  T t17 = T( u );
  T t18 = T( v );
  T t19 = T( w );
  T t20 = T( x );
  return 0;
}
