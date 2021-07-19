// DR 2126
// { dg-do compile { target c++11 } }

typedef const int CI[3];
constexpr CI &ci = CI{11, 22, 33};
static_assert(ci[1] == 22, "");
