// PR c++/78771
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fnew-inheriting-ctors" }

// ICE instantiating a deleted inherited ctor

struct Base
{
  template <typename U> Base (U);

  Base (int);
};

struct Derived;

struct Middle : Base
{
  using Base::Base;

  Middle (Derived);
};

struct Derived : Middle
{
  using Middle::Middle;
};

Middle::Middle (Derived) : Middle (0) {}
