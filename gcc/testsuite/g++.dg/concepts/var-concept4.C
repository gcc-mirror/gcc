// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T, typename U>
concept bool Same = __is_same_as(T, U);

template<typename T0, typename T1, typename T2, typename... T3toN>
concept bool Same<T0, T1, T2, T3toN...> = true; // { dg-error "wrong number|does not match" }

template<typename T>
concept bool C1 = true;

template<typename T>
concept bool C1<T*> = true; // { dg-error "specialization of variable concept" }

template<typename T>
concept bool C2 = true;

template<>
concept bool C2<int> = true; // { dg-error "non-template variable" }
