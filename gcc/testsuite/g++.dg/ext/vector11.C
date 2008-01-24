// PR c++/34913

template<typename T> struct A
{
  int x[sizeof(T)] __attribute((vector_size(8)));
};
