// PR c++/91618

template <class T> int f(T t, int)
{ return t.i; }

class A {
  friend int ::f(A);		// { dg-error "" }
  int i;
};

int main()
{
  f(A());			// { dg-error "no match" }
}
