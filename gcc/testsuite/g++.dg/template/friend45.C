// PR c++/28025

class BaseSubmit
{
  template<class T> friend class PeriodicSubmit;
};

template<class ID>
class ValuesSubmit 
{
  template<class T> friend class PeriodicSubmit;
};

class A;
class MultiSubmit : public ValuesSubmit<A>
{
};
