// { dg-do run { target c++11 } }

struct A { int i,j; };

struct X {
  A a = {0,0};
};

constexpr X table[2][2] = {{ {} }};

#define SA(X) static_assert(X,#X)
SA(table[1][1].a.i == 0);

extern "C" void abort();

const int *p = &table[1][1].a.j;

int main()
{
  if (*p != 0)
    abort();
}
