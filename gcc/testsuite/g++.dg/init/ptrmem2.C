// PR c++/19916
// { dg-do run }

struct S {
  char k;
};

char const volatile S::* const p01 = &S::k;
int main(void)
{
  return  0;
}
