// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Sept 2001 <nathan@codesourcery.com>

// Bug 4203. We were bit copying empty bases including the
// padding. Which clobbers whatever they overlay.

class EmptyBase0 {};
class EmptyBase1 : public EmptyBase0 {};
class Base1
{
public:
unsigned int t_;
Base1(unsigned int t) : t_(t) {}
};

class PEPE : public Base1, public EmptyBase1
{
public:
PEPE(unsigned int t)
  : Base1(t), EmptyBase1(EmptyBase1()) {}
};

int main()
{
  PEPE pepe(0xff);
  
  return pepe.t_ != 255;
}
