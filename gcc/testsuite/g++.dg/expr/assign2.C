// PR c++/85141
// { dg-options "-w -fpermissive" }

struct A
{
  static int foo();
};

void bar(int i)
{
  i += A().foo;
}
