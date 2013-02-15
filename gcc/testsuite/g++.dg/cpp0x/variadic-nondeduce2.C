// PR c++/55220
// { dg-do compile { target c++11 } }

template <typename ...> struct something_like_tuple
{

};

template <typename, typename> struct is_last
{
  static const bool value = false;
};

// Head is non-deducible, so this can't work as the user intended
template <typename T, template <typename ...> class Tuple, typename ... Head>
struct is_last<T, Tuple<Head ..., T>>
{
  static const bool value = true;
};

#define SA(X) static_assert (X, #X)

typedef something_like_tuple<char, int, float> something_like_tuple_t;
SA ((is_last<float, something_like_tuple_t>::value == false));
SA ((is_last<int, something_like_tuple_t>::value == false));
