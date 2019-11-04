// { dg-do compile { target concepts } }

template <class T, class U> concept NotSame = !__is_same_as (T, U);

template <class T, class X>
struct A
{
  template <NotSame<A> U> void f(U) { }
  template <class U> void f(U);
};

int main()
{
  A<int,int>().f<char>(0);
}
