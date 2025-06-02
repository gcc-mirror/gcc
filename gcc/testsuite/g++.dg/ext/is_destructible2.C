// PR c++/107600
// { dg-additional-options -Wno-c++17-extensions }
// { dg-do compile { target c++11 } }

struct A
{
  A& operator= (const A&);
  virtual ~A() = 0;
};

static_assert( __is_destructible(A) );
static_assert( __is_assignable(A, A) );
static_assert( not __is_destructible(int()) );
static_assert( not __is_nothrow_destructible(int()) );
static_assert( not __is_trivially_destructible(int()) );
