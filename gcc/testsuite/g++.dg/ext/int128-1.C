// PR c++/50454
// { dg-do compile { target int128 } }

template<typename T>
  struct limits;

template<>
  struct limits<__int128> { }; // { dg-error "does not support" }

template<>
  struct limits<unsigned __int128> { }; // { dg-error "does not support" }
