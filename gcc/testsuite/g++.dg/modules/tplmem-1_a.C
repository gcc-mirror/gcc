// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module frob;
// { dg-module-cmi "frob" }

export struct A
{
  A ()
  {
  }

  template<typename T> operator T () const
  {
    return T(99);
  }
};

export template<typename T> struct B
{
  T m;

  B(T t) : m(t) 
  {
  }

  template<typename S> operator S () const
  {
    return S (m);
  }
};

