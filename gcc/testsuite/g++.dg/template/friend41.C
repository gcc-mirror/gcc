// PR c++/26912

struct Foo { 
  template<class T> int func() const; 
}; 

class Bar { 
  friend int Foo::func<int>() const;
}; 


