// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Jan 2005 <nathan@codesourcery.com>

// PR 19270: ICE
// Origin:  Ralf Wildenhues <Ralf.Wildenhues@gmx.de>

template<class T> struct Vec {
  T* data;
  T& operator[](int i) const;
};

template<class T> inline T& Vec<T>::operator[](int i) const
{
  return (&data[0])[i];
}

inline double foo(Vec<double> v)
{
  return v[0];
}
