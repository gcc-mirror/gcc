// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local" }
// { dg-module-cmi M }
// Ignore exposures in these cases

export module M;

namespace {
  inline namespace ns {
    struct internal_t {};
    template <typename T> struct internal_tmpl_t {};

    int internal_x;
    void internal_ovl(int&) {}
    void internal_ovl(internal_t) {}

    template <typename T> void internal_tmpl() {}
  }
}
export struct ok_inst_tag {};


// The function body for a non-inline function or function template
export void function() {
  internal_t i {};
  internal_tmpl_t<int> ii {};
  internal_ovl(internal_x);
  internal_tmpl<int>();
}

export template <typename T> void function_tmpl() {
  internal_t i {};
  internal_tmpl_t<T> ii {};
  internal_ovl(internal_x);
  internal_tmpl<T>();
}
template void function_tmpl<ok_inst_tag>();
template <> void function_tmpl<ok_inst_tag*>() {}


// The initializer for a variable or variable template
export int var
  = (internal_t{}, internal_tmpl_t<int>{},
     internal_ovl(internal_x), internal_tmpl<int>(), 0);

export template <typename T> int var_tmpl
  = (internal_t{}, internal_tmpl_t<T>{},
     internal_ovl(internal_x), internal_tmpl<T>(), 0);

template <typename T> int var_tmpl<T*>  // { dg-warning "refers to TU-local entity" }
  = (internal_t{}, internal_tmpl_t<T*>{},
     internal_ovl(internal_x), internal_tmpl<T*>(), 0);

template int var_tmpl<ok_inst_tag>;
template <> int var_tmpl<ok_inst_tag*> = 0;

export int& constant_ref = internal_x;
static_assert (&constant_ref == &internal_x);


// Friend declarations in a class definition
export struct klass {  // { dg-bogus "TU-local" }
  friend ns::internal_t;
  friend ns::internal_tmpl_t<int>;
  friend void ns::internal_ovl(int&);
  friend void ns::internal_ovl(internal_t);
  friend void ns::internal_tmpl<int>();

  template <typename> friend struct ns::internal_tmpl_t;
  template <typename> friend void ns::internal_tmpl();
};

export template <typename T>
class klass_tmpl {  // { dg-bogus "TU-local" }
  friend ns::internal_t;
  friend ns::internal_tmpl_t<int>;
  friend void ns::internal_ovl(int&);
  friend void ns::internal_ovl(internal_t);
  friend void ns::internal_tmpl<int>();

  template <typename> friend struct ns::internal_tmpl_t;
  template <typename> friend void ns::internal_tmpl();
};

template <typename T> class klass_tmpl<T*> {  // { dg-bogus "TU-local" }
  friend ns::internal_t;
  friend ns::internal_tmpl_t<int>;
  friend void ns::internal_ovl(int&);
  friend void ns::internal_ovl(internal_t);
  friend void ns::internal_tmpl<int>();

  template <typename> friend struct ns::internal_tmpl_t;
  template <typename> friend void ns::internal_tmpl();
};


// Any reference to a non-volatile const object or reference with internal or
// no linkage initialized with a constant expression that is not an ODR-use
static const int value = 123;
static const int& ref = 456;
static const internal_t internal {};
void f(int) {}
export inline void no_odr_use() {
  int x = value;
  int y = ref;
  int z = (internal, 0);

  value;
  bool b = value < value;
  f(value);
}
