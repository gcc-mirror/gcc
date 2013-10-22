// { dg-options "-std=c++11" }

struct GrandParent {
  void *get();
};

template<class OBJ>
struct Parent : public GrandParent{
};

template<typename T>
struct Child : public Parent<T> {
  using GrandParent::get;
  void Foo() {
    void* ex = get();
  }
};
