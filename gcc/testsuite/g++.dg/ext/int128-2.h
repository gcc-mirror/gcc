#pragma GCC system_header

template<typename T>
  struct limits;

template<>
  struct limits<__int128> { };

template<>
  struct limits<unsigned __int128> { };
