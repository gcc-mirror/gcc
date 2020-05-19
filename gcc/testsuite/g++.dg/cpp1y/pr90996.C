// PR c++/90996
// { dg-do run { target c++14 } }

struct S
{
  int &&a = 2;
  int b[1] {a};
};

S c[2][2] {{{5}}};

struct T
{
  S c[2][2] {{{7}}};
};

T d {};

int
main()
{
  if (++c[0][0].b[0] != 6
      || ++c[0][1].b[0] != 3
      || ++c[1][0].b[0] != 3
      || ++c[1][1].b[0] != 3)
    __builtin_abort();

  auto& e = d.c;
  if (++e[0][0].b[0] != 8
      || ++e[0][1].b[0] != 3
      || ++e[1][0].b[0] != 3
      || ++e[1][1].b[0] != 3)
    __builtin_abort();
}
