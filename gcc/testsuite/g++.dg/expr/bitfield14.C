// PR c++/30277
// { dg-do compile { target c++11 } }

struct S
{
  signed long l: 32;
};

void foo(long) = delete;
void foo(int) {}

int main()
{
  S x = {1};
  foo(x.l+0);
  return 0;
}
