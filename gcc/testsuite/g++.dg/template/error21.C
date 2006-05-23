// PR c++/20173

template<typename AT>
struct A{
  template<typename T>
  void function(T){}
};

template<>
template<typename T>
void A<int>::function(T){}

template<>
template<typename T>
void A<double>::function(T*){} // { dg-error "match" }
