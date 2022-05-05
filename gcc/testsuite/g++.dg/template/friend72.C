// PR c++/91618
// { dg-do link }

template <class T> int f(T t)
{ return t.i; }

class A {
  friend int ::f(A);
  int i;
};

int main()
{
  f(A()); // link error, trying to call non-template function
}
