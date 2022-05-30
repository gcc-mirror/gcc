// PR c++/105725
// { dg-do compile { target c++14 } }
// { dg-options "-Wall -Wmismatched-tags" }

template <bool> struct enable_if;
template <bool Cond> using enable_if_t = typename enable_if<Cond>::type;
template <typename> bool is_class_v;
template <class, class> bool B;
template <class T>
bool B<T, enable_if_t<is_class_v<class T::foo>>>;
