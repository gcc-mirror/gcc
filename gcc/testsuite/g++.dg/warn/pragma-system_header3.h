#pragma GCC system_header

template <typename T>
  int* g() { static char c; return reinterpret_cast<int*>(&c); }

template <typename T>
  T* h() { static char c; return reinterpret_cast<T*>(&c); }
