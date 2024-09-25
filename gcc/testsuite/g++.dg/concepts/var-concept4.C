// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T, typename U>
concept Same = __is_same_as(T, U);

template<typename T0, typename T1, typename T2, typename... T3toN>
concept Same<T0, T1, T2, T3toN...> = true; // { dg-error "expected" }

template<typename T>
concept C1 = true;

template<typename T>
concept C1<T*> = true; // { dg-error "expected" }

template<typename T>
concept C2 = true;

template<>
concept C2<int> = true; // { dg-error "concept definition syntax|expected|type" }
