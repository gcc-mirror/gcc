// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;

template <typename _MemFunPtr>
class _Mem_fn_base {
  template <typename> friend struct _Bind_check_arity;
};

template <typename> struct _Bind_check_arity {};

export module M;

template struct _Bind_check_arity<int>;
export _Mem_fn_base<int> mem_fn();
