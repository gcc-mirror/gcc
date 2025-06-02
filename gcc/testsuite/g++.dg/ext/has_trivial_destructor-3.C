// { dg-do compile { target c++11 } }

struct X;

template<class T>
struct default_delete
{
  void operator()(T*) { static_assert(sizeof(T), "type is not incomplete"); }
};

template<class T, class D = default_delete<T>>
struct unique_ptr
{
  ~unique_ptr() { del(ptr); }

  T* ptr;
  D del;
};


constexpr bool b = __has_trivial_destructor(unique_ptr<X>);
