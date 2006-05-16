// PR c++/27339

class A 
{
private: 
  enum private_enum {a};

  template<A::private_enum v>  // OK 
  struct B
  {
    void bm();
  }; 
public: 
  void am() 
  { 
    B<a> instance; //OK
    instance.bm();
  }
};

template<A::private_enum v>  // FAIL
void
A::B<v>::bm(){}
