// PR c++/102434
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof 0);

namespace std {
  template<typename T> union initializer_list { // { dg-error "definition of .*std::initializer_list.* does not match" }
    const T *ptr;
    size_t n;
  };
}
template<typename T>
void Task() {}
auto b = { &Task<int> };

// { dg-prune-output "compilation terminated" }
