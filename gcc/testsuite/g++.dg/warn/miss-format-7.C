// PR c++/90953 - ICE with -Wmissing-format-attribute.
// { dg-do compile { target c++11 } }
// { dg-options "-Wmissing-format-attribute" }

namespace std {
union [[gnu::may_alias]] _Any_data{};
enum _Manager_operation { __get_type_info };
template <typename> class A;
class B {
  typedef bool (*_Manager_type)(_Any_data, const _Any_data &,
                                _Manager_operation);
public:
  _Any_data _M_functor;
  _Manager_type _M_manager;
};

struct type_info { } t;

template <typename _Res, typename... _ArgTypes>
class A<_Res(_ArgTypes...)> : B {
  const type_info &target_type() const noexcept;
};
template <typename _Res, typename... _ArgTypes>
const type_info &A<_Res(_ArgTypes...)>::target_type() const noexcept {
  _Any_data __typeinfo_result;
  _M_manager(__typeinfo_result, _M_functor, __get_type_info);
  return t;
}
} // namespace std
