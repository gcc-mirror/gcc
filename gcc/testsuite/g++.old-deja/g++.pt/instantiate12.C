// { dg-do run  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// Bug 635. We failed to emit initializer code for out-of-class defined
// static const members of template instantiations.

static int inited = 0;

static bool setFlag()
{
  inited++;
  return true;
}

template<typename T> struct X
{
  static const bool cflag;
  static bool flag;
  static const bool iflag = true;
  static const bool jflag = true;
};

template<typename T> const bool X<T>::cflag (setFlag ());
template<typename T> bool X<T>::flag (setFlag ());
template<typename T> const bool X<T>::iflag;

int main ()
{
  X<int> a;
  if (!a.flag)
    return 1;
  if (!a.cflag)
    return 2;
  if (!a.iflag)
    return 3;
  if (!a.jflag)
    return 5;
  if (!X<float>::flag)
    return 5;
  if (!X<float>::cflag)
    return 6;
  if (!X<float>::iflag)
    return 7;
  if (!X<float>::jflag)
    return 8;
  if (inited != 4)
    return 9;
  return 0;
}

// On platforms that do not have weak symbols, these static data
// members must be explicitly instantiated.  The iflag and jflag data
// members should not have to be explicitly instantiated because their
// const-ness should allow the compiler to elide references to the
// actual variables.
template const bool X<int>::cflag;
template bool X<int>::flag;
template const bool X<float>::cflag;
template bool X<float>::flag;
