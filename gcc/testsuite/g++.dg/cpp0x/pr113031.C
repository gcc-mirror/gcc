// PR c++/113031
// { dg-do compile }

template <typename> struct variant;

template <typename _Types, typename _Tp>
variant<_Types> __variant_cast(_Tp __rhs) { return static_cast<variant<_Types>&>(__rhs); }

template <typename _Types>
struct _Move_assign_base : _Types {
  void operator=(_Move_assign_base __rhs) { __variant_cast<_Types>(__rhs); }
};

template <typename _Types>
struct variant : _Move_assign_base<_Types> {
  void emplace() {
    variant __tmp;
    *this = __tmp;
  }
};

struct _Undefined_class {
  struct _Nocopy_types {
    void (_Undefined_class::*_M_member_pointer)();
  };
  struct function : _Nocopy_types {
    struct optional {
      void test03() {
        variant<function> v;
        v.emplace();
      }
    };
  };
};
