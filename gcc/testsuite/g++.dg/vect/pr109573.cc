// { dg-do compile }
// { dg-require-effective-target c++20 }

void *operator new(__SIZE_TYPE__, void *__p) { return __p; }
template <typename _Head> struct _Head_base {
  _Head _M_head_impl;
};
template <unsigned long, typename...> struct _Tuple_impl;
template <unsigned long _Idx, typename _Head, typename... _Tail>
struct _Tuple_impl<_Idx, _Head, _Tail...> : _Tuple_impl<_Idx + 1, _Tail...>,
                                            _Head_base<_Head> {
  template <typename _UHead, typename... _UTail>
  _Tuple_impl(_UHead __head, _UTail... __tail)
      : _Tuple_impl<_Idx + 1, _Tail...>(__tail...), _Head_base<_Head>(__head) {}
};
template <unsigned long _Idx, typename _Head> struct _Tuple_impl<_Idx, _Head> {
  template <typename _UHead> _Tuple_impl(_UHead);
};
template <typename... _Elements> struct tuple : _Tuple_impl<0, _Elements...> {
  template <typename... _UElements>
  tuple(_UElements... __elements)
      : _Tuple_impl<0, _Elements...>(__elements...) {}
};
unsigned long position_;
struct Zone {
  template <typename T, typename... Args> T *New(Args... args) {
    return new (reinterpret_cast<void *>(position_)) T(args...);
  }
};
struct Label {
  int pos_;
  int near_link_pos_;
};
enum Condition { below_equal };
void bind(Label *);
Zone *zone();
unsigned long deopt_info_address();
int MakeDeferredCode___trans_tmp_2, MakeDeferredCode___trans_tmp_3,
    Prologue___trans_tmp_6, MakeDeferredCode___trans_tmp_1;
struct MaglevAssembler {
  template <typename Function, typename... Args>
  void MakeDeferredCode(Function &&, Args &&...);
  template <typename Function, typename... Args>
  void JumpToDeferredIf(Condition, Function, Args... args) {
    MakeDeferredCode(Function(), args...);
  }
  void Prologue();
};
struct ZoneLabelRef {
  ZoneLabelRef(Zone *zone) : label_(zone->New<Label>()) {}
  ZoneLabelRef(MaglevAssembler *) : ZoneLabelRef(zone()) {}
  Label *operator*() { return label_; }
  Label *label_;
};
template <typename Function>
struct FunctionArgumentsTupleHelper
    : FunctionArgumentsTupleHelper<decltype(&Function::operator())> {};
template <typename C, typename R, typename... A>
struct FunctionArgumentsTupleHelper<R (C::*)(A...) const> {
  using Tuple = tuple<A...>;
};
template <typename> struct StripFirstTupleArg;
template <typename T1, typename... T>
struct StripFirstTupleArg<tuple<T1, T...>> {
  using Stripped = tuple<T...>;
};
template <typename Function> struct DeferredCodeInfoImpl {
  template <typename... InArgs>
  DeferredCodeInfoImpl(int *, int, int, Function, InArgs... args)
      : args(args...) {}
  StripFirstTupleArg<
      typename FunctionArgumentsTupleHelper<Function>::Tuple>::Stripped args;
};
template <typename Function, typename... Args>
void MaglevAssembler::MakeDeferredCode(Function &&deferred_code_gen,
                                       Args &&...args) {
  zone()->New<DeferredCodeInfoImpl<Function>>(
      &MakeDeferredCode___trans_tmp_1, MakeDeferredCode___trans_tmp_2,
      MakeDeferredCode___trans_tmp_3, deferred_code_gen, args...);
}
void MaglevAssembler::Prologue() {
  int *__trans_tmp_9;
  ZoneLabelRef deferred_call_stack_guard_return(this);
  __trans_tmp_9 = reinterpret_cast<int *>(deopt_info_address());
  JumpToDeferredIf(
      below_equal, [](MaglevAssembler, int *, ZoneLabelRef, int, int) {},
      __trans_tmp_9, deferred_call_stack_guard_return, Prologue___trans_tmp_6,
      0);
  Label __trans_tmp_7 = **deferred_call_stack_guard_return;
  bind(&__trans_tmp_7);
}
