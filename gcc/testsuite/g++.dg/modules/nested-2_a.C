// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module bob;
// { dg-module-cmi bob }

export struct X
{
  typedef X *iter;

  int m;
  X() :m(-1)
  {
  }
  
  void set (int m_)
  {
    m = m_;
  }
  operator int () const
  {
    return m;
  }
};
