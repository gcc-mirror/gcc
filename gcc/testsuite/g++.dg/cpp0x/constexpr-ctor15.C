// PR c++/57694
// { dg-do compile { target c++11 } }

class A
{
private:
  int a;
  const int* const aptr;

public:
  constexpr A(int _a) : a(_a), aptr(&a) { }
};

class Data { } d1;

class B
{
private:
  Data* dptr1;

public:
  constexpr B(Data* _p) : dptr1(_p) {}
};

class Use
{
  static constexpr A a{2};
  static constexpr B b{&d1};
};
