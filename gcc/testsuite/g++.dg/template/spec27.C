// PR c++/24139

template<typename T>
struct O {
  struct I;
};

template<>
struct O<int>::I
{
  I();
};

O<int>::I::I() {}
