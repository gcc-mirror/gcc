// This should fail deduction, before it produces a candidate.
// { dg-do compile { target c++11 } }

template <class... T>
void f(T... ts);		// { dg-message "deduction" }

struct B { };
int main()
{
  f<int>(B(), 1);		// { dg-error "" }
}
