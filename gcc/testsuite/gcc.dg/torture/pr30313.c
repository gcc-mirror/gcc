/* { dg-do compile } */

static inline void bar(){}

struct S
{
  signed int i: 32;
};

int main()
{
  struct S x = {32};
  sizeof(x.i+0);
  return 0;
}
