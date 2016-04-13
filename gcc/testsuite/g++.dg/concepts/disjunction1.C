// PR c++/66962
// { dg-options "-std=c++1z -fconcepts" }

template <typename> struct remove_cv;
template <typename> struct is_reference;
template <typename> void declval();
template <typename> struct is_constructible;
template <typename> struct is_nothrow_constructible;
template <typename _Tp> using remove_cv_t = typename remove_cv<_Tp>::type;
template <typename> struct Trans_NS_extension_apply_list;
template <typename T> using _t = typename T::type;
template <class> void ImplicitlyConvertibleTo();
template <class> void Assignable();
template <class T, class... Args> int ConstructibleObject = requires { T{}; };
template <class T, class... Args>
concept bool BindableReference =
    is_reference<T>::value &&is_constructible<T>::value;
template <class T, class... Args> concept bool Constructible() {
  return ConstructibleObject<T> || BindableReference<T, Args...>;
}
template <class T> concept bool DefaultConstructible() {
  return Constructible<T>() && requires { new T[0]; };
}
template <class T> concept bool MoveConstructible() {
  return Constructible<T>() && ImplicitlyConvertibleTo<T>;
}
template <class T> concept bool Movable() {
  return MoveConstructible<T>() && Assignable<T &&>;
}
template <class, class> int Swappable_ = requires { 0; };
template <class T, class U> int Swappable();
template <class T> concept bool Dereferencable = requires{{0}};
template <Dereferencable R> using RvalueReferenceType = decltype(0);
template <class T> int IsValueType;
template <class> struct value_type;
template <class T>
requires IsValueType<
    _t<value_type<remove_cv_t<T>>>> using ValueType =
    _t<value_type<remove_cv_t<T>>>;
template <class I> concept bool Readable() {
  return Movable<I>() && DefaultConstructible<I>() &&
         Dereferencable<const I> && requires{{0}};
}
template <class Out, class T> concept bool MoveWritable() {
  return Movable<Out>() && DefaultConstructible<Out>() &&
         Dereferencable<Out>;
}
template <class In, class Out> concept bool IndirectlyMovable() {
  return Readable<In>() && Movable<ValueType<In>>() &&
         Constructible<ValueType<In>>() &&
         MoveWritable<Out, RvalueReferenceType<In>>() &&
         MoveWritable<Out, ValueType<In>>();
}
IndirectlyMovable { In, Out }
int is_nothrow_indirectly_movable_v =
    is_nothrow_constructible<ValueType<In>>::value;
template <Readable R1, Readable R2>
    requires IndirectlyMovable<R1, R2>() &&
    IndirectlyMovable<R2, R1>() void iter_swap2();
