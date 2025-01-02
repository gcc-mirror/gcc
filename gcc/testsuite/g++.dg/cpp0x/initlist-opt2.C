// PR c++/116476
// { dg-do compile { target c++11 } }

namespace std {
template <typename T>
class initializer_list {
  T *_M_len;
  __SIZE_TYPE__ size;
};
} // namespace std


template <class T>
struct field {
    field(T &&) {}
};
struct vector {
  vector(std::initializer_list<field<int>>) { }
};

vector fields_normal{2};
