/* { dg-do compile } */
/* { dg-require-effective-target c++17 } */
/* { dg-options "-O2 -mavx2 -mtune=haswell" } */

template <typename _Tp> struct remove_reference {
  using type = __remove_reference(_Tp);
};
template <typename T> struct MaybeStorageBase {
  T val;
  struct Union {
    ~Union();
  } mStorage;
};
template <typename T> struct MaybeStorage : MaybeStorageBase<T> {
  char mIsSome;
};
template <typename T, typename U = typename remove_reference<T>::type>
constexpr MaybeStorage<U> Some(T &&);
template <typename T, typename U> constexpr MaybeStorage<U> Some(T &&aValue) {
  return {aValue};
}
template <class> struct Span {
  int operator[](long idx) {
    int *__trans_tmp_4;
    if (__builtin_expect(idx, 0))
      *(int *)__null = false;
    __trans_tmp_4 = storage_.data();
    return __trans_tmp_4[idx];
  }
  struct {
    int *data() { return data_; }
    int *data_;
  } storage_;
};
struct Variant {
  template <typename RefT> Variant(RefT) {}
};
long from_i, from___trans_tmp_9;
namespace js::intl {
struct DecimalNumber {
  Variant string_;
  unsigned long significandStart_;
  unsigned long significandEnd_;
  bool zero_ = false;
  bool negative_;
  template <typename CharT> DecimalNumber(CharT string) : string_(string) {}
  template <typename CharT>
  static MaybeStorage<DecimalNumber> from(Span<const CharT>);
  void from();
};
} // namespace js::intl
void js::intl::DecimalNumber::from() {
  Span<const char16_t> __trans_tmp_3;
  from(__trans_tmp_3);
}
template <typename CharT>
MaybeStorage<js::intl::DecimalNumber>
js::intl::DecimalNumber::from(Span<const CharT> chars) {
  DecimalNumber number(chars);
  if (auto ch = chars[from_i]) {
    from_i++;
    number.negative_ = ch == '-';
  }
  while (from___trans_tmp_9 && chars[from_i])
    ;
  if (chars[from_i])
    while (chars[from_i - 1])
      number.zero_ = true;
  return Some(number);
}

/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-32,\[^\\n\]*sp" } } */
