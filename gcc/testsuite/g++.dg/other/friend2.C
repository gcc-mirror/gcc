// { dg-do run }
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>

// PR c++/12370
// Wrong code because of the friend declaration

template <typename T> struct A
{
    T x;
    A(T t) : x(t) {}
    friend A<int> foo (const A<unsigned>&);
};

A<int> foo (const A<unsigned>& a)
{
    A<int> res(a.x);
    return res;
}

int main()
{
    return foo(A<unsigned>(0)).x;
}
