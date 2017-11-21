// { dg-module-do run }

export module bob;
// { dg-module-bmi bob }

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
