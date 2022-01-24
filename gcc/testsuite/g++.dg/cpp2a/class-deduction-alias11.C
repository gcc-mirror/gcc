// PR c++/102643
// { dg-do compile { target c++20 } }

template<class _Tp, class>
struct vector {
  typedef int allocator_type;
  vector(_Tp, allocator_type = allocator_type());
};

template<class T> using vector_mm = vector<T, int>;

vector_mm v(0);
