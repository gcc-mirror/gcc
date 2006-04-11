struct T; // { dg-error "forward" }
T* manage(T* t);
template <class Obj> struct ObjectSlot0_ {
  void create() {
    void* tmp = manage(new T()); // { dg-error "incomplete" }
  }
};
