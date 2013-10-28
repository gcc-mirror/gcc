// { dg-options -std=c++11 }

#include <type_traits>

#define IS_LIT(T) (std::is_literal_type<T>::value)
#define SA(X) static_assert (X, #X)
#define YES(T) SA(IS_LIT(T))
#define NO(T) SA(!IS_LIT(T))

enum E1 { };
enum class E2 { };
struct Literal {};

struct NotLiteral {
  ~NotLiteral();
};

YES(int);
YES(int[]);
YES(int[3]);
YES(double);
YES(void *);
YES(decltype (nullptr));
YES(int Literal::*);
YES(void (Literal::*)());
YES(E1);
YES(E2);
YES(Literal);
NO (NotLiteral);
YES(NotLiteral *);
YES(NotLiteral NotLiteral::*);
YES(NotLiteral (NotLiteral::*)(NotLiteral));

struct A {
  A(const A&) = default;
  A(int);
};

NO(A);				// no constexpr ctor other than copy
