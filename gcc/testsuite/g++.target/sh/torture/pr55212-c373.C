/* { dg-additional-options "-std=c++20 -mlra -fpic -w " }  */
/* { dg-do compile }  */

      namespace std {
     typedef unsigned int size_t;
     }
       namespace std {
     __attribute__((__always_inline__)) constexpr inline bool __is_constant_evaluated() noexcept {
   return __builtin_is_constant_evaluated();
   }
     }
      typedef unsigned int size_t;
       extern "C" {
     typedef struct {
   }
     lldiv_t;
     extern "C" {
   typedef unsigned char __uint8_t;
   typedef unsigned int __uint32_t;
   typedef union {
 struct {
 }
 __value32;
 }
   __atomic_wide_counter;
   }
     }
      extern "C++" {
     namespace std __attribute__ ((__visibility__ ("default"))) {
   }
     }
       extern "C++" {
     namespace std __attribute__ ((__visibility__ ("default"))) {
   template<typename _Tp, _Tp __v> struct integral_constant {
 static constexpr _Tp value = __v;
 using value_type = _Tp;
 constexpr operator value_type() const noexcept {
 }
 };
   template<bool __v> using __bool_constant = integral_constant<bool, __v>;
   using true_type = __bool_constant<true>;
   using false_type = __bool_constant<false>;
   template<bool, typename _Tp = void> struct enable_if {
 };
   template<typename _Tp> struct enable_if<true, _Tp> {
 using type = _Tp;
 };
   template<bool> struct __conditional {
 };
   template<bool _Cond, typename _If, typename _Else> using __conditional_t = typename __conditional<_Cond>::template type<_If, _Else>;
   template <typename _Type> struct __type_identity {
 };
   namespace __detail {
 template<typename... _Bn> auto __and_fn(...) -> false_type;
 }
   template<typename... _Bn> struct __and_ : decltype(__detail::__and_fn<_Bn...>(0)) {
 };
   template<typename> struct remove_cv;
   template <typename _Tp, size_t = sizeof(_Tp)> constexpr true_type __is_complete_or_unbounded(__type_identity<_Tp>) {
 return {
};
 }
   template<typename _Tp> struct is_void : public false_type {
 };
   template<typename _Tp> struct is_member_object_pointer : public __bool_constant<__is_member_object_pointer(_Tp)> {
 };
   template<typename _Tp> struct is_member_function_pointer : public __bool_constant<__is_member_function_pointer(_Tp)> {
 };
   template<typename _Tp> struct is_empty : public __bool_constant<__is_empty(_Tp)> {
 };
   template<typename _Tp> _Tp __declval(long);
   template<typename _Tp> auto declval() noexcept -> decltype(__declval<_Tp>(0));
   template<typename _Tp, typename... _Args> using __is_constructible_impl = __bool_constant<__is_constructible(_Tp, _Args...)>;
   template<typename _Tp> using __add_lval_ref_t = __add_lvalue_reference(_Tp);
   template<typename _Tp> struct is_copy_constructible : public __is_constructible_impl<_Tp, __add_lval_ref_t<const _Tp>> {
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{
}
), "template argument must be a complete class or an unbounded array");
 };
   template<typename _Tp> using __add_rval_ref_t = __add_rvalue_reference(_Tp);
   template<typename _Tp> struct is_move_constructible : public __is_constructible_impl<_Tp, __add_rval_ref_t<_Tp>> {
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{
}
), "template argument must be a complete class or an unbounded array");
 };
   template<typename _Tp, typename _Up> using __is_assignable_impl = __bool_constant<__is_assignable(_Tp, _Up)>;
   template<typename _Tp> struct is_copy_assignable : public __is_assignable_impl<__add_lval_ref_t<_Tp>, __add_lval_ref_t<const _Tp>> {
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{
}
), "template argument must be a complete class or an unbounded array");
 };
   template<typename _Tp, typename _Up> struct is_same : public __bool_constant<__is_same(_Tp, _Up)> {
 };
   template<typename _Tp> struct remove_const {
 };
   template<typename _Tp> struct add_const {
 };
   template<typename _Tp> struct remove_reference {
 };
   template<typename _Tp> using remove_reference_t = typename remove_reference<_Tp>::type;
   template<typename _Tp> struct add_pointer {
 };
   template<typename _Tp> using __remove_cvref_t = typename remove_cv<typename remove_reference<_Tp>::type>::type;
   template<bool _Cond, typename _Iftrue, typename _Iffalse> struct conditional {
 };
   struct __failure_type {
 };
   struct __invoke_memfun_ref {
 };
   struct __invoke_other {
 };
   template<typename _Tp, typename _Up = __remove_cvref_t<_Tp>> struct __inv_unwrap {
 };
   template<bool, bool, typename _Functor, typename... _ArgTypes> struct __result_of_impl {
 };
   template<typename _Functor, typename... _ArgTypes> struct __invoke_result : public __result_of_impl< is_member_object_pointer< typename remove_reference<_Functor>::type >::value, is_member_function_pointer< typename remove_reference<_Functor>::type >::value, _Functor, _ArgTypes... >::type {
 };
   template<typename _Result, typename _Ret, bool = is_void<_Ret>::value, typename = void> struct __is_invocable_impl : false_type {
 };
   template<typename _Fn, typename _Tp, typename... _Args> constexpr bool __call_is_nt(__invoke_memfun_ref) {
 using _Up = typename __inv_unwrap<_Tp>::type;
 return noexcept((std::declval<_Up>().*std::declval<_Fn>())( std::declval<_Args>()...));
 }
   template<typename _Result, typename _Fn, typename... _Args> struct __call_is_nothrow : __bool_constant< std::__call_is_nt<_Fn, _Args...>(typename _Result::__invoke_type{
}
  ) > {
 };
   template<typename _Ret, typename _Fn, typename... _ArgTypes> struct is_invocable_r : __is_invocable_impl<__invoke_result<_Fn, _ArgTypes...>, _Ret>::type {
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Fn>{
}
), "_Fn must be a complete class or an unbounded array");
 static_assert((std::__is_complete_or_unbounded( __type_identity<_ArgTypes>{
}
) && ...), "each argument type must be a complete class or an unbounded array");
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Ret>{
}
), "_Ret must be a complete class or an unbounded array");
 };
   template <typename _Tp> inline constexpr bool is_lvalue_reference_v = false;
   template <typename _Tp> inline constexpr bool is_enum_v = __is_enum(_Tp);
   template <typename _Tp> inline constexpr bool is_union_v = __is_union(_Tp);
   template <typename _Tp> inline constexpr bool is_class_v = __is_class(_Tp);
   template <typename _Tp> inline constexpr bool is_trivial_v = __is_trivial(_Tp);
   template <typename _Tp> inline constexpr bool is_standard_layout_v = __is_standard_layout(_Tp);
   template <typename _Tp, typename... _Args> inline constexpr bool is_constructible_v = __is_constructible(_Tp, _Args...);
   template <typename _Tp, typename _Up> inline constexpr bool is_same_v = __is_same(_Tp, _Up);
   template <typename _Base, typename _Derived> inline constexpr bool is_base_of_v = __is_base_of(_Base, _Derived);
   template <typename _From, typename _To> inline constexpr bool is_convertible_v = __is_convertible(_From, _To);
   template<typename... _Tp> struct common_reference;
   template<typename... _Tp> using common_reference_t = typename common_reference<_Tp...>::type;
   }
     }
       namespace std __attribute__ ((__visibility__ ("default"))) {
     template<typename _Tp> inline constexpr _Tp* __addressof(_Tp& __r) noexcept {
   }
     template<typename _Tp> [[__nodiscard__]] constexpr typename std::remove_reference<_Tp>::type&& move(_Tp&& __t) noexcept {
   }
     namespace __detail {
   template<typename _Tp, typename _Up> concept __same_as = std::is_same_v<_Tp, _Up>;
   }
     template<typename _Tp, typename _Up> concept same_as = __detail::__same_as<_Tp, _Up> && __detail::__same_as<_Up, _Tp>;
     template<typename _From, typename _To> concept convertible_to = is_convertible_v<_From, _To> && requires {
   static_cast<_To>(std::declval<_From>());
   };
     template<typename _Tp, typename _Up> concept common_reference_with = same_as<common_reference_t<_Tp, _Up>, common_reference_t<_Up, _Tp>> && convertible_to<_Tp, common_reference_t<_Tp, _Up>> && convertible_to<_Up, common_reference_t<_Tp, _Up>>;
     namespace __detail {
   template<typename _Tp> using __cref = const remove_reference_t<_Tp>&;
   template<typename _Tp> concept __class_or_enum = is_class_v<_Tp> || is_union_v<_Tp> || is_enum_v<_Tp>;
   template<typename _Tp> constexpr bool __destructible_impl = false;
   template<typename _Tp> requires requires(_Tp& __t) {
 {
 __t.~_Tp() }
 noexcept;
 }
   constexpr bool __destructible_impl<_Tp> = true;
   template<typename _Tp> constexpr bool __destructible = __destructible_impl<_Tp>;
   }
     template<typename _Lhs, typename _Rhs> concept assignable_from = is_lvalue_reference_v<_Lhs> && common_reference_with<__detail::__cref<_Lhs>, __detail::__cref<_Rhs>> && requires(_Lhs __lhs, _Rhs&& __rhs) {
   {
 __lhs = static_cast<_Rhs&&>(__rhs) }
   -> same_as<_Lhs>;
   };
     template<typename _Tp> concept destructible = __detail::__destructible<_Tp>;
     template<typename _Tp, typename... _Args> concept constructible_from = destructible<_Tp> && is_constructible_v<_Tp, _Args...>;
     template<typename _Tp> concept default_initializable = constructible_from<_Tp> && requires {
   (void) ::new _Tp;
   };
     template<typename _Tp> concept move_constructible = constructible_from<_Tp, _Tp> && convertible_to<_Tp, _Tp>;
     namespace ranges {
   namespace __swap {
 template<typename _Tp, typename _Up> concept __adl_swap = (std::__detail::__class_or_enum<remove_reference_t<_Tp>> || std::__detail::__class_or_enum<remove_reference_t<_Up>>) && requires(_Tp&& __t, _Up&& __u) {
 swap(static_cast<_Tp&&>(__t), static_cast<_Up&&>(__u));
 };
 struct _Swap {
 private: template<typename _Tp, typename _Up> static constexpr bool _S_noexcept() {
 if constexpr (__adl_swap<_Tp, _Up>) return noexcept(swap(std::declval<_Tp>(), std::declval<_Up>()));
 }
 public: template<typename _Tp, typename _Up> requires __adl_swap<_Tp, _Up> || (same_as<_Tp, _Up> && is_lvalue_reference_v<_Tp> && move_constructible<remove_reference_t<_Tp>> && assignable_from<_Tp, remove_reference_t<_Tp>>) constexpr void operator()(_Tp&& __t, _Up&& __u) const noexcept(_S_noexcept<_Tp, _Up>()) {
 if constexpr (__adl_swap<_Tp, _Up>) swap(static_cast<_Tp&&>(__t), static_cast<_Up&&>(__u));
 else {
 }
 }
 template<typename _Tp, typename _Up, size_t _Num> requires requires(const _Swap& __swap, _Tp& __e1, _Up& __e2) {
 __swap(__e1, __e2);
 }
 constexpr void operator()(_Tp (&__e1)[_Num], _Up (&__e2)[_Num]) const noexcept(noexcept(std::declval<const _Swap&>()(*__e1, *__e2))) {
 for (size_t __n = 0;
 __n < _Num;
 ++__n) (*this)(__e1[__n], __e2[__n]);
 }
 };
 }
   inline namespace _Cpo {
 inline constexpr __swap::_Swap swap{
};
 }
   }
     template<typename _Tp> concept swappable = requires(_Tp& __a, _Tp& __b) {
   ranges::swap(__a, __b);
   };
     template<typename _Tp, typename _Up> concept swappable_with = common_reference_with<_Tp, _Up> && requires(_Tp&& __t, _Up&& __u) {
   ranges::swap(static_cast<_Tp&&>(__t), static_cast<_Tp&&>(__t));
   };
     namespace __detail {
   template<typename _Tp> concept __boolean_testable_impl = convertible_to<_Tp, bool>;
   template<typename _Tp> concept __boolean_testable = __boolean_testable_impl<_Tp> && requires(_Tp&& __t) {
 {
 !static_cast<_Tp&&>(__t) }
 -> __boolean_testable_impl;
 };
   template<typename _Tp, typename _Up> concept __weakly_eq_cmp_with = requires(__detail::__cref<_Tp> __t, __detail::__cref<_Up> __u) {
 {
 __t == __u }
 -> __boolean_testable;
 {
 __u == __t }
 -> __boolean_testable;
 {
 __u != __t }
 -> __boolean_testable;
 };
   }
     namespace __detail {
   template<typename _Tp, typename _Up> concept __partially_ordered_with = requires(const remove_reference_t<_Tp>& __t, const remove_reference_t<_Up>& __u) {
 {
 __t < __u }
 -> __boolean_testable;
 {
 __t <= __u }
 -> __boolean_testable;
 {
 __t >= __u }
 -> __boolean_testable;
 {
 __u < __t }
 -> __boolean_testable;
 };
   }
     template<typename _Tp> struct __get_first_arg {
   };
     template<typename _Ptr, typename = void> struct __ptr_traits_elem : __get_first_arg<_Ptr> {
   };
     template<typename _Ptr> requires requires {
   typename _Ptr::element_type;
   }
     struct __ptr_traits_elem<_Ptr, void> {
   };
     template<typename _Ptr, typename _Elt, bool = is_void<_Elt>::value> struct __ptr_traits_ptr_to {
   using pointer = _Ptr;
   using element_type = _Elt;
   static pointer pointer_to(element_type& __r) requires requires {
 {
 pointer::pointer_to(__r) }
 -> convertible_to<pointer>;
 }
   {
 }
   };
     namespace __detail {
   template<typename _Tp> using __with_ref = _Tp&;
   template<typename _Tp> concept __can_reference = requires {
 typename __with_ref<_Tp>;
 };
   template<typename _Tp> concept __dereferenceable = requires(_Tp& __t) {
 {
 *__t }
 -> __can_reference;
 };
   }
     namespace ranges {
   namespace __imove {
 template<typename _Tp> concept __adl_imove = (std::__detail::__class_or_enum<remove_reference_t<_Tp>>) && requires(_Tp&& __t) {
 iter_move(static_cast<_Tp&&>(__t));
 };
 struct _IterMove {
 private: template<typename _Tp> struct __result {
 };
 template<typename _Tp> static constexpr bool _S_noexcept() {
 if constexpr (__adl_imove<_Tp>) return noexcept(iter_move(std::declval<_Tp>()));
 }
 public: template<std::__detail::__dereferenceable _Tp> using __type = typename __result<_Tp>::type;
 template<std::__detail::__dereferenceable _Tp> [[nodiscard]] constexpr __type<_Tp> operator()(_Tp&& __e) const noexcept(_S_noexcept<_Tp>()) {
 }
 };
 }
   }
     template<typename> struct incrementable_traits {
   };
     namespace __detail {
   }
     template<size_t __i, typename _Tp> struct tuple_element;
     template<size_t __i, typename _Tp> using __tuple_element_t = typename tuple_element<__i, _Tp>::type;
     template<typename _Tp, typename... _Types> constexpr size_t __find_uniq_type_in_pack() {
   constexpr size_t __sz = sizeof...(_Types);
   constexpr bool __found[__sz] = {
 __is_same(_Tp, _Types) ... };
   size_t __n = __sz;
   for (size_t __i = 0;
   __i < __sz;
   ++__i) {
 if (__found[__i]) {
 if (__n < __sz) return __sz;
 }
 }
   };
     template<typename _Tp, _Tp... _Idx> struct integer_sequence {
   };
     template<typename _Tp, _Tp _Num> using make_integer_sequence = integer_sequence<_Tp, __integer_pack(_Num)...>;
     template<size_t... _Idx> using index_sequence = integer_sequence<size_t, _Idx...>;
     template<size_t _Num> using make_index_sequence = make_integer_sequence<size_t, _Num>;
     template<typename... _Types> using index_sequence_for = make_index_sequence<sizeof...(_Types)>;
     template<typename _Tp> struct in_place_type_t {
   };
     template<typename...> class tuple;
     template<typename _U1, typename _U2> class __pair_base {
   };
     namespace __detail {
   template<typename _Iterator> struct __move_iter_cat {
 };
   }
     template<typename _Iterator> class move_iterator : public __detail::__move_iter_cat<_Iterator> {
   };
     template<typename _IteratorL, typename _IteratorR> [[__nodiscard__]] inline constexpr bool operator<(const move_iterator<_IteratorL>& __x, const move_iterator<_IteratorR>& __y) requires requires {
   {
 __x.base() < __y.base() }
   -> convertible_to<bool>;
   }
     {
   return !(__x < __y);
   }
     namespace __ops {
   template<typename _Compare> struct _Iter_comp_iter {
 _Compare _M_comp;
 explicit constexpr _Iter_comp_iter(_Compare __comp) : _M_comp(std::move(__comp)) {
 }
 template<typename _Iterator1, typename _Iterator2> constexpr bool operator()(_Iterator1 __it1, _Iterator2 __it2) {
 return bool(_M_comp(*__it1, *__it2));
 }
 };
   template<typename _Compare> constexpr inline _Iter_comp_iter<_Compare> __iter_comp_iter(_Compare __comp) {
 }
   }
     template<typename _Tp> [[__nodiscard__]] constexpr inline const _Tp& min(const _Tp& __a, const _Tp& __b) {
   return __a;
   }
     template<typename _Tp> [[__nodiscard__]] constexpr inline const _Tp& max(const _Tp& __a, const _Tp& __b) {
   if (__a < __b) return __b;
   }
     template<typename _Iterator, typename _Predicate> constexpr inline _Iterator __find_if(_Iterator __first, _Iterator __last, _Predicate __pred) {
   }
     }
       namespace std __attribute__ ((__visibility__ ("default"))) {
     struct allocator_arg_t {
   struct _Sink {
 void constexpr operator=(const void*) {
 }
 }
   _M_a;
   };
     template<typename _Res, typename _Fn, typename... _Args> constexpr _Res __invoke_impl(__invoke_other, _Fn&& __f, _Args&&... __args) {
   }
     template<class _E> class initializer_list {
   typedef size_t size_type;
   typedef const _E* iterator;
   private: iterator _M_array;
   size_type _M_len;
   };
     template<typename _Tp> struct __is_empty_non_tuple : is_empty<_Tp> {
   };
     template<typename _Tp> using __empty_not_final = __conditional_t<__is_final(_Tp), false_type, __is_empty_non_tuple<_Tp>>;
     template<size_t _Idx, typename _Head, bool = __empty_not_final<_Head>::value> struct _Head_base;
     template<size_t _Idx, typename... _Elements> struct _Tuple_impl;
     template<size_t _Idx, typename _Head, typename... _Tail> struct _Tuple_impl<_Idx, _Head, _Tail...> : public _Tuple_impl<_Idx + 1, _Tail...>, private _Head_base<_Idx, _Head> {
   };
     template<size_t __i, typename... _Elements> constexpr __tuple_element_t<__i, tuple<_Elements...>>& get(tuple<_Elements...>& __t) noexcept {
   }
     template<typename... _Tps, typename... _Ups> requires (sizeof...(_Tps) == sizeof...(_Ups)) && (requires (const _Tps& __t, const _Ups& __u) {
   {
 __t == __u }
   -> __detail::__boolean_testable;
   }
     && ...) constexpr bool operator== [[nodiscard]] (const tuple<_Tps...>& __t, const tuple<_Ups...>& __u) {
   return [&]<size_t... _Inds>(index_sequence<_Inds...>) {
 }
  (index_sequence_for<_Tps...>{
}
  );
   }
     }
       typedef struct {
     union {
   }
     __value;
     }
       __mbstate_t;
       namespace std __attribute__ ((__visibility__ ("default"))) {
     }
       static inline int __gthread_yield (void) {
     }
      namespace std __attribute__ ((__visibility__ ("default"))) {
     template <typename _Tp, size_t _Nm> [[nodiscard, __gnu__::__always_inline__]] constexpr _Tp* data(_Tp (&__array)[_Nm]) noexcept {
   };
     }
      typedef __uint8_t uint8_t;
       typedef __uint32_t uint32_t;
      namespace std {
     namespace __detail {
   inline void __thread_yield() noexcept {
 }
   inline void __thread_relax() noexcept {
 }
   inline constexpr auto __atomic_spin_count = 16;
   struct __default_spin_policy {
 bool operator()() const noexcept {
 }
 };
   template<typename _Pred, typename _Spin = __default_spin_policy> bool __atomic_spin(_Pred& __pred, _Spin __spin = _Spin{
 }
  ) noexcept {
 for (auto __i = 0;
 __i < __atomic_spin_count;
 ++__i) {
 }
 }
   }
     struct __numeric_limits_base {
   };
     template<typename _Tp> struct numeric_limits : public __numeric_limits_base {
   static constexpr bool is_signed = true;
   static constexpr bool is_integer = true;
   };
     template<> struct numeric_limits<unsigned int> {
   static constexpr bool is_signed = false;
   static constexpr bool is_integer = true;
   };
     }
       namespace WTF {
     template <typename T> struct IsSmartPtr {
   };
     template <typename ExpectedType, typename ArgType, bool isBaseType = std::is_base_of_v<ExpectedType, ArgType>> struct TypeCastTraits {
   };
     template<typename T> class TypeHasRefMemberFunction {
   };
     }
       namespace WTF {
     }
       namespace std {
     namespace experimental {
   };
     }
       namespace std __attribute__ ((__visibility__ ("default"))) {
     template<typename _Tp, std::size_t _Nm> struct array {
   };
     }
      extern "C" {
     extern float sqrtf (float __x) noexcept (true);
     extern float floorf (float __x) noexcept (true) __attribute__ ((__const__));
     namespace std __attribute__ ((__visibility__ ("default"))) {
   constexpr bool isnan(float __x) {
 return __builtin_isnan(__x);
 }
   }
     }
       constexpr float piFloat = static_cast<float>( 3.14159265358979323846 );
       template<typename TargetType, typename SourceType> typename std::enable_if<!std::is_same<TargetType, SourceType>::value && std::numeric_limits<SourceType>::is_integer && std::numeric_limits<TargetType>::is_integer && std::numeric_limits<TargetType>::is_signed && !std::numeric_limits<SourceType>::is_signed && sizeof(SourceType) >= sizeof(TargetType), TargetType>::type clampTo(SourceType value) {
     return static_cast<TargetType>(value);
     }
       namespace WTF {
     constexpr uint32_t roundUpToPowerOfTwo(uint32_t v) {
   return v;
   };
     template<bool isPod, typename T> struct VectorTraitsBase;
     template<typename T> struct VectorTraits : VectorTraitsBase<std::is_standard_layout_v<T> && std::is_trivial_v<T>, T> {
   };
     }
       namespace WTF {
     class StringHasher {
   };
     enum class DestructionThread : uint8_t {
   Any, Main, MainRunLoop };
     }
       namespace WebCore {
     class IntSize {
   public: constexpr IntSize() = default;
   void setWidth(int width) {
 m_width = width;
 }
   void setHeight(int height) {
 m_height = height;
 }
   private: int m_width {
 0 };
   int m_height {
 0 };
   };
     inline IntSize& operator+=(IntSize& a, const IntSize& b) {
   }
     }
       namespace WebCore {
     class FloatSize {
   public: constexpr FloatSize() = default;
   constexpr float width() const {
 return m_width;
 }
   constexpr float height() const {
 return m_height;
 }
   private: float m_width {
 0 };
   float m_height {
 0 };
   };
     }
       namespace WTF {
     class ThreadSafeRefCountedBase {
   };
     template<typename T, DestructionThread destructionThread = DestructionThread::Any> class ThreadSafeRefCountedAndCanMakeThreadSafeWeakPtr {
   };
     }
       using WTF::ThreadSafeRefCountedAndCanMakeThreadSafeWeakPtr;
       namespace WebCore {
     class FloatPoint {
   };
     inline FloatSize toFloatSize(const FloatPoint& a) {
   }
     }
       namespace WTF {
     }
       namespace WebCore {
     template<typename T, size_t N> struct ColorComponents {
   template<typename F> constexpr auto map(F&& function) const -> ColorComponents<decltype(function(std::declval<T>())), N>;
   };
     template<typename> struct AlphaTraits;
     inline constexpr ColorComponents<float, 4> resolveColorComponents(const ColorComponents<float, 4>& colorComponents) {
   return colorComponents.map([] (float component) {
 return std::isnan(component) ? 0.0f : component;
 }
  );
   };
     enum class WhitePoint {
   D50, D65 };
     template<typename Parent> struct ColorWithAlphaHelper {
   };
     template<typename T, typename D, typename ColorType, typename M, typename TF> struct RGBAType : ColorWithAlphaHelper<ColorType> {
   constexpr RGBAType(T red, T green, T blue, T alpha = AlphaTraits<T>::opaque) : red {
 red }
   {
 }
   protected: T red;
   };
     }
       namespace WebCore {
     class RenderingResource : public ThreadSafeRefCountedAndCanMakeThreadSafeWeakPtr<RenderingResource> {
   };
     class FilterFunction : public RenderingResource {
   };
     class FilterEffect : public FilterFunction {
   };
     class AffineTransform {
   public: constexpr AffineTransform();
   private: std::array<double, 6> m_transform;
   };
     constexpr AffineTransform::AffineTransform() : m_transform {
   }
     {
   }
     }
       namespace WTF {
     }
       namespace WebCore {
     class FEGaussianBlur : public FilterEffect {
   static IntSize calculateUnscaledKernelSize(FloatSize stdDeviation);
   };
     }
       namespace WTF {
     }
       namespace WebCore {
     static inline float gaussianKernelFactor() {
   return 3 / 4.f * sqrtf(2 * piFloat);
   }
     static int clampedToKernelSize(float value) {
   static constexpr unsigned maxKernelSize = 500;
   unsigned size = std::max<unsigned>(2, static_cast<unsigned>(floorf(value * gaussianKernelFactor() + 0.5f)));
   return clampTo<int>(std::min(size, maxKernelSize));
   }
     IntSize FEGaussianBlur::calculateUnscaledKernelSize(FloatSize stdDeviation) {
   IntSize kernelSize;
   if (stdDeviation.width()) kernelSize.setWidth(clampedToKernelSize(stdDeviation.width()));
   if (stdDeviation.height()) kernelSize.setHeight(clampedToKernelSize(stdDeviation.height()));
   return kernelSize;
   }
     }
