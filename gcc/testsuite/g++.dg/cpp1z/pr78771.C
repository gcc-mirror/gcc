// PR c++/78771
// { dg-options -std=c++1z }

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
