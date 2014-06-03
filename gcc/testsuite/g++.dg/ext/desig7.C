// PR c++/61046

struct A
{
  int ary[4];
};
const int i = 0;
A bar = { [i] = 0 }; // { dg-error "designated" }
