// { dg-options "-std=gnu++0x" }
// A tuple type
template<typename... Args> struct tuple { };

// Determine if two types are the same
template<typename T, typename U>
struct is_same {
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

// Append 'T' to the end of Tuple
template<typename T, typename Tuple>
struct append_to_tuple;

template<typename T, typename... Args>
struct append_to_tuple<T, tuple<Args...> > {
  typedef tuple<Args..., T> type;
};

// Reverse a sequence of arguments (and return the result as a tuple)
template<typename... Args> struct reverse;

template<typename T, typename... Args>
struct reverse<T, Args...> {
  typedef typename append_to_tuple<T, typename reverse<Args...>::type>::type
    type;
};

template<>
struct reverse<> {
  typedef tuple<> type;
};

int a0[is_same<reverse<>::type, tuple<> >::value? 1 : -1];
int a1[is_same<reverse<int>::type, tuple<int> >::value? 1 : -1];
int a2[is_same<reverse<char, int>::type, tuple<int, char> >::value? 1 : -1];
int a3[is_same<reverse<char, int, long>::type, tuple<long, int, char> >::value? 1 : -1];
