// PR c++/14912

template <class T, int N=0, int X=1>
struct A
{
};

void foo(void)
{
  A<void> a = 0;		// { dg-error "A<void>" }
}
