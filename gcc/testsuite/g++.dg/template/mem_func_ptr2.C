// PR c++/85356

struct A
{
  A& operator=(int);
};

void foo(A&(A::*)(int));

template<int> void bar()
{
  foo(&A::operator=);
}
