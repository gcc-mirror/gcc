// { dg-do compile { target c++11 } }
template<typename... Args> struct tuple1 { };
template<typename... Args> struct tuple2 { };

template<typename T, typename U>
struct same_tuple_args {
  static const bool value = false;
};

template<typename... Args>
struct same_tuple_args<tuple1<Args...>, tuple2<Args...> > {
  static const bool value = true;
};

int same0[same_tuple_args<tuple1<>, tuple2<> >::value? 1 : -1];
int same1[same_tuple_args<tuple1<int>, tuple2<int> >::value? 1 : -1];
int same2[same_tuple_args<tuple1<float, int>, tuple2<float, int> >::value? 1 : -1];
int diff0[!same_tuple_args<tuple1<>, tuple2<int> >::value? 1 : -1];
int diff1[!same_tuple_args<tuple1<int, float>, tuple2<float, int> >::value? 1 : -1];
