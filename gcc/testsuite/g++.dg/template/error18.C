// PR c++/20157

template<typename AT>
struct A{
  template<typename T>
  void function(T);
};

template<>
template<typename ABC,typename DEF>
void A<int>::function(ABC); // { dg-error "match" }
