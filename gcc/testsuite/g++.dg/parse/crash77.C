// PR c++/117158
// { dg-do "compile" }

struct Base {
  unsigned int *intarray;
};

template <typename T>
struct Sub : public Base {
  bool Get(int i) {
    return (Base::intarray[++i] == 0);
  }
};
