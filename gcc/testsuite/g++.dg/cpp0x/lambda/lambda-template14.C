// PR c++/62219
// { dg-do compile { target c++11 } }

template< class = void >
struct S
{
  friend void foo( S )
  {
    [](){};
  }
};
