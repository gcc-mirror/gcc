// { dg-do compile { target c++11 } }
template<typename... Args = int> // { dg-error "default argument" }
class tuple2;

template<typename... = int> // { dg-error "default argument" }
class tuple3;

template<typename T1, typename T2, typename... Rest>
struct two_or_more {}; // { dg-message "provided for" }

typedef two_or_more<int> bad; // { dg-error "at least 2" "at least 2" }

void f()
{
  two_or_more<int, float> z = 5; // { dg-error "two_or_more<int, float>" }
}
