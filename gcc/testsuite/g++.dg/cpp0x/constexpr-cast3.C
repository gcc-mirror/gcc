// PR c++/99176
// { dg-do compile { target c++11 } }

constexpr const int *p = nullptr;
constexpr int *q1 = const_cast<int*>(p);
constexpr int *q2 = (int *)(const int *) nullptr;

struct B { };
struct D : B { };
constexpr B *q3 = static_cast<B*>(nullptr);
constexpr D *pd = nullptr;
constexpr B *pb = nullptr;
constexpr B *q4 = static_cast<B*>(pd);
constexpr D *q5 = static_cast<D*>(pb);
