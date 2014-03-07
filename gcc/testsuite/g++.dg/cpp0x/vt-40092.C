// { dg-do compile { target c++11 } }

template <typename... Types> struct package {};

template <int ArgGen> struct wrapper_gen {};

template <int ArgNest> struct wrapper_nest
{
  typedef wrapper_gen<ArgNest> type_nest;
};

template <int... ArgPack>
struct wrapper_pack
{
  typedef package<wrapper_gen <ArgPack>...> type_pack;
  // incorrect error: expansion pattern 'wrapper_gen<ArgNest>'
  //    contains no argument packs
};


