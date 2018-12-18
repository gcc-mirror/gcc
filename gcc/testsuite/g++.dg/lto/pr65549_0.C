// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -std=gnu++14 -flto -g -Wno-return-type } { -std=gnu++14 -flto -g -O2 -fno-inline -flto-partition=max -Wno-return-type } } }
// { dg-extra-ld-options "-r -nostdlib" }

namespace std {
inline namespace __cxx11 {}
template <typename _Tp, _Tp> struct integral_constant {
  static constexpr _Tp value = 0;
};
template <typename> struct __and_;
struct is_member_object_pointer : integral_constant<bool, false> {};
template <typename>
struct is_member_function_pointer : integral_constant<bool, false> {};
template <typename> struct remove_reference { typedef int type; };
template <typename> class C;
template <bool, int, typename...> struct __result_of_impl;
template <typename _Functor, typename... _ArgTypes>
struct __result_of_impl<false, 0, _Functor, _ArgTypes...> {
  typedef decltype(0) type;
};
template <typename _Functor, typename... _ArgTypes>
struct C<_Functor(_ArgTypes...)>
    : __result_of_impl<is_member_object_pointer::value,
                       is_member_function_pointer<
                           typename remove_reference<_Functor>::type>::value,
                       _Functor> {};
template <typename _Tp> using result_of_t = typename C<_Tp>::type;
template <typename> void forward() { }
template <typename _Tp> _Tp move(_Tp) {}
namespace __cxx11 {
class basic_string typedef string;
}
template <typename> struct allocator_traits { typedef decltype(0) pointer; };
}
struct F : std::allocator_traits<int> {};
namespace std {
namespace __cxx11 {
class basic_string {
public:
  struct _Alloc_hider : F {
    _Alloc_hider(pointer);
  } _M_dataplus;
  basic_string(int) : _M_dataplus(0) {}
  ~basic_string();
};
}
template <typename> class function;
template <typename _Functor> class _Base_manager {
protected:
  static _Functor *_M_get_pointer(int) {}
};
template <typename, typename> class _Function_handler;
template <typename _Res, typename _Functor, typename... _ArgTypes>
class _Function_handler<_Res(_ArgTypes...), _Functor>
    : _Base_manager<_Functor> {
public:
  static _Res _M_invoke(const int &) {
    (*_Base_manager<_Functor>::_M_get_pointer(0))();
  }
};
template <typename, typename> using __check_func_return_type = int;
template <typename _Res, typename... _ArgTypes>
class function<_Res(_ArgTypes...)> {
  template <typename> using _Invoke = decltype(0);
  template <typename _Functor>
  using _Callable = __and_<__check_func_return_type<_Invoke<_Functor>, _Res>>;
  template <typename, typename> using _Requires = int;

public:
  template <typename _Functor, typename = _Requires<_Callable<_Functor>, void>>
  function(_Functor);
  using _Invoker_type = _Res (*)(const int &);
  _Invoker_type _M_invoker;
};
template <typename _Res, typename... _ArgTypes>
template <typename _Functor, typename>
function<_Res(_ArgTypes...)>::function(_Functor) {
  _M_invoker = _Function_handler<_Res(), _Functor>::_M_invoke;
}
class unique_ptr {
public:
  ~unique_ptr();
};
template <typename _Tp, typename... _Args> _Tp make_unique(_Args... __args) {
  _Tp(__args...);
}
}
class A {
public:
  template <class T> T as();
};
class variables_map {
public:
  A operator[](std::basic_string);
};
class B {
public:
  variables_map configuration();
  void run(int, int, std::function<void()>);
};
class H;
struct G {
  enum {} _state;
};
class D {
  G _local_state;
  std::unique_ptr _task;
  template <typename Func> void schedule(Func func) {
    struct task_with_state {
      task_with_state(Func func) : _func(func) {}
      Func _func;
    } tws = std::make_unique<task_with_state>(std::move(func));
  }
  friend H;
};
template <typename> using futurize_t = H;
class H {
  D *_promise;
  template <typename Func> void schedule(Func func) {
    G __trans_tmp_1;
    struct task_with_ready_state {
      task_with_ready_state(Func, G) { };
    };
    std::make_unique<task_with_ready_state>(std::move(func), __trans_tmp_1);
    _promise->schedule(std::move(func));
  }
  template <typename Func, typename Param> void then(Func func, Param) {
    using P = D;
    P pr;
    schedule([ pr = std::move(pr), func, param = std::forward<Param> ]{});
  }

public:
  template <typename Func> futurize_t<std::result_of_t<Func()>> then(Func) {
    then(0, [] {});
  }
} clients;
int main() {
  B app;
  app.run(0, 0, [&] {
    auto config = app.configuration()[0].as<std::string>();
    clients.then([] {});
  });

  return 0;
}
