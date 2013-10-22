// { dg-options -std=c++11 }

template<class T>
constexpr T value_init() { return T(); }

template<class T>
constexpr inline T bar(T x) { return x; }

union EmptyUnion {};
union Union1 { int i; };
union Union3 { double d; int i; char* c; };

constexpr auto u1 = value_init<EmptyUnion>();
constexpr auto u2 = value_init<Union1>();
constexpr auto u3 = value_init<Union3>();
constexpr auto u4 = bar(EmptyUnion{});
constexpr auto u5 = bar(Union1{});
constexpr auto u6 = bar(Union3{});
constexpr auto u7 = bar(u1);
