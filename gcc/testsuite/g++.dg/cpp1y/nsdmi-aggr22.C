// PR c++/114854
// { dg-do compile { target c++14 } }

struct Vector {
  int m_size;
};
struct S {
  const Vector &vec{};
};

void spawn(S);
void test() { spawn({}); }
