// PR c++/108887
// { dg-do compile { target c++11 } }

template <int __v> struct integral_constant {
  static constexpr int value = __v;
};
using false_type = integral_constant<false>;
template <bool, bool, typename...> struct __result_of_impl;
template <typename _Functor, typename... _ArgTypes>
struct __result_of_impl<false, false, _Functor, _ArgTypes...> {
  typedef decltype(0) type;
};
template <typename... _ArgTypes>
struct __invoke_result
    : __result_of_impl<false_type::value, false_type::value, _ArgTypes...> {};
template <typename, typename _Fn, typename... _Args>
void __invoke_impl(_Fn __f, _Args... __args) {
  __f(__args...);
}
template <typename, typename _Callable, typename... _Args>
void __invoke_r(_Callable __fn, _Args... __args) {
  using __result = __invoke_result<_Args...>;
  using __type = typename __result::type;
  __invoke_impl<__type>(__fn, __args...);
}
struct QString {
  QString(const char *);
};
template <typename> class function;
template <typename _Functor> struct _Base_manager {
  static _Functor _M_get_pointer(int) { __builtin_abort (); }
};
template <typename, typename> class _Function_handler;
template <typename _Res, typename _Functor, typename... _ArgTypes>
struct _Function_handler<_Res(_ArgTypes...), _Functor> {
  using _Base = _Base_manager<_Functor>;
  static _Res _M_invoke(const int &__functor, _ArgTypes &&...__args) {
    auto __trans_tmp_1 = _Base::_M_get_pointer(__functor);
    __invoke_r<_Res>(__trans_tmp_1, __args...);
  }
};
template <typename _Res, typename... _ArgTypes>
struct function<_Res(_ArgTypes...)> {
  template <typename _Functor>
  using _Handler = _Function_handler<_Res(_ArgTypes...), _Functor>;
  template <typename _Functor> function(_Functor) {
    using _My_handler = _Handler<_Functor>;
    _M_invoker = _My_handler::_M_invoke;
  }
  using _Invoker_type = _Res (*)(const int &, _ArgTypes &&...);
  _Invoker_type _M_invoker;
};
struct QRegularExpression {
  QRegularExpression(QString);
};
struct AbstractAccount {
  void get(function<void(AbstractAccount *)>,
           function<void(AbstractAccount *)>);
};
struct AbstractTimelineModel {
  AbstractAccount m_account;
};
struct LinkPaginationTimelineModel : AbstractTimelineModel {
  void fillTimeline();
};
void LinkPaginationTimelineModel::fillTimeline() {
  [] {};
  m_account.get([](AbstractAccount *) { static QRegularExpression re(""); },
                [](AbstractAccount *) {});
}
