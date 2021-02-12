// PR c++/97246
// { dg-do compile { target c++20 } }

template <int... Is, typename T>
T arg_T(decltype(Is)..., T, ...);

template <int I, int... Is>
inline constexpr auto get =
 []<typename... T>(decltype(Is)..., T... v, ...) {
   static_assert( sizeof...(T) == sizeof...(v) );
   if constexpr ( sizeof...(T) == 1 )
     return (v,...);
   else {
     using V = decltype(arg_T<__integer_pack(I)...>(v...));
     return get<I,__integer_pack(I)...>.template operator()<V>(v...);
   }
 };

static_assert( get<0>('\0', short{1}, 2, long{3}) == 0 );
