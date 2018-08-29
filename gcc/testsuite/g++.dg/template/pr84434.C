// PR c++/84434 ICE with deduction guide and dependent using decl
// { dg-do compile { target c++17 } }

template <typename T> class B {
public:
  template <typename U> B (U)  {}
};

template <typename T>
struct scope_guard : B<T> {
  using base_type = B<T>;

  using base_type::base_type;

   ~scope_guard() = default;
};

template <typename T>
scope_guard (T) -> scope_guard<T>;

void Frob () {
  scope_guard (1);
}
