// PR target/128437
// { dg-do compile { target { *-*-linux* && { ! ia32 } } } }
// { dg-options "-O2 -fPIC -fvisibility=hidden -mtls-dialect=gnu" }
// { dg-require-effective-target c++20 }

template <typename _Tp> constexpr bool is_pointer_v = __is_pointer(_Tp);
template <typename _Tp> constexpr bool is_function_v = __is_function(_Tp);
void allocation_prefix();
int is_pointer___trans_tmp_3;
long bits_;
struct {
  static void *slot_load(void *) {
    return is_pointer___trans_tmp_3 ? reinterpret_cast<void *>(bits_) : nullptr;
  }
} *entries_, source;
void erase(void *slot) {
  while (entries_)
    if (entries_ == slot) {
      entries_ = {};
      return;
    }
}
int thread_local current;
void released(void *slot) {
  if (current)
    erase(slot);
}
template <class T>
concept SlotValue = !is_function_v<T>;
template <SlotValue T> void *allocation(T value) {
  if constexpr (is_pointer_v<T>)
    return value;
  else
    return T::slot_load(&value);
}
struct Reference {
  int value_;
  Reference() { released(&value_); }
  void detach() { released(&value_); }
};
template <SlotValue T> void replace(T slot, Reference next) {
  next.detach();
  void *__trans_tmp_2 = allocation(slot);
  if (__trans_tmp_2)
    allocation_prefix();
}
int *views;
void retire_owned() {
  replace(source, {});
  replace(views, {});
}
