// PR c++/120577
// { dg-do compile { target c++20 } }

template <class _Tp> struct optional {
  union {
    _Tp __val_;
  };
  template <class... _Args>
  constexpr optional(_Args... __args)
      : __val_(__args...) {}
};
template <class _Tp, class... _Args>
constexpr optional<_Tp> make_optional(_Args... __args) {
  return optional<_Tp>(__args...);
}

struct __non_trivial_if {
  constexpr __non_trivial_if() {}
};
struct allocator : __non_trivial_if {};
struct __padding {};
struct __short {
  [[__no_unique_address__]] __padding __padding_;
  int __data_;
};
struct basic_string {
  union {
    __short __s;
  };
  [[__no_unique_address__]] allocator __alloc_;
  constexpr basic_string(int, int) {}
};
auto opt = make_optional<basic_string>(4, 'X');
