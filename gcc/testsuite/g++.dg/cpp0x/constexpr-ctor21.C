// PR c++/83835
// { dg-do compile { target c++11 } }

struct Z
{
  void const * p_;
  constexpr Z( void const * p ): p_( p ) {}
  ~Z();
};

struct Y
{
  Z z_;
  constexpr Y() noexcept: z_( this ) {}
};
