// PR c++/61382
// { dg-do run { target c++11 } }

struct A
{
  int i,j;
  A(int i,int j):i(i),j(j){}
};

extern "C" int printf (const char *, ...);

int main()
{
  int i = 0;
  A a{i++,i++};
  if (a.i != 0 || a.j != 1)
    __builtin_abort();
}
