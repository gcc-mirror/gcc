// { dg-do run  }
// Testcase for use of template parms as types for other template parms.

template <class T, T t>
class A {
    T	a;
public:
    A(): a(t) {}

    operator T () { return a; }
};

template <class S, S s>
class B {
    A<S,s> a;
public:
    B(A<S,s>& b): a(b) {}

    operator S () { return a*20; }
};

int
main()
{
    A<int, 5> a;
    B<int, 5> b(a);

    if (b * a == 500)
      return 0;
    else
      return 1;
}
