namespace N
{

template <typename> struct A
{
  template <typename T> A(A<T>);
};

}

void foo(N::A<int>);

void bar()
{
  foo(N::A); // { dg-error "" }
}
