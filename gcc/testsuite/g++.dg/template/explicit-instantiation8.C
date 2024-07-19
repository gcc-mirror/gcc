// PR c++/109464
// { dg-do compile { target c++11 } }

template<typename T>
struct shallow
 {
   int len;
   constexpr shallow() : len(0) { }
  };

template<typename T>
struct use_shallow
  {
   static constexpr shallow<T> s_zstr = { };
   static_assert(s_zstr.len == 0, "");
  };

extern template struct shallow<char>;
extern template struct use_shallow<char>;

template struct shallow<char>;
template struct use_shallow<char>;

// { dg-final { scan-assembler "_ZN7shallowIcEC2Ev" } }
