// { dg-do compile }
// { dg-options "-Wall" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Sep 2003 <nathan@codesourcery.com>

// PR 12037.

struct X
{
  int operator+(int);
  int operator-(int);
  int operator*(int);
  int operator/(int);
  int operator%(int);
  int operator>>(int);
  int operator<<(int);
  int operator&(int);
  int operator|(int);
  int operator^(int);
  int operator&&(int);
  int operator||(int);
  int operator==(int);
  int operator!=(int);
  int operator<(int);
  int operator<=(int);
  int operator>(int);
  int operator>=(int);
  int operator*();
  int operator!();
  int operator~();
  int operator++();
  int operator--();
  int operator++(int);
  int operator--(int);
  int operator()();
  int operator,(int);
  X *operator->();
  operator int () const;
  int m;
  virtual ~X ();
  X &Foo ();
};
struct Y : X 
{
};

template<int I> void Foo (X &x)
{
  x + I;
  x - I;
  x * I;
  x / I;
  x % I;
  x >> I;
  x << I;
  x & I;
  x | I;
  x && I;
  x || I;
  x == I;
  x != I;
  x < I;
  x <= I;
  x > I;
  x >= I;
  *x;
  !x;
  ~x;
  x++;
  x--;
  ++x;
  --x;
  x ();
  x, I;
  x->m;
  static_cast<int> (x);
  dynamic_cast<Y &> (x);
  reinterpret_cast<int> (x.Foo ());
  const_cast<X &> (x.Foo ());
  
  reinterpret_cast<int *> (&x);// { dg-warning "no effect" "" }
  const_cast<X &> (x);         // { dg-warning "no effect" "" }
  sizeof (x++);                // { dg-warning "no effect" "" }
  __alignof__ (x++);           // { dg-warning "no effect" "" }
}
  

