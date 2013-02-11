// PR c++/56238

class A
{
  template < typename T > T& get ();
  template < typename T > class B
  {
    void RemovePoint (A& value)
    {
      static_cast < double >(value.get < T > ());
    }
  };
};
