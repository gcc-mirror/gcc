// PR c++/48500
// { dg-options -std=c++0x }

struct linked_ptr {
};
template <typename T> linked_ptr make_linked_ptr(T* ptr);
struct Concrete;
struct NewedClass {
  NewedClass(const Concrete& req){}
};
template<typename ArgT> void AddObjToChange(const ArgT& req) {
  linked_ptr p = make_linked_ptr(new NewedClass(req));
}
