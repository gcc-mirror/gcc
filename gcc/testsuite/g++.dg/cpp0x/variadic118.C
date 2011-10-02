// This should fail deduction, before it produces a candidate.
// { dg-options -std=c++0x }

template <class... T>
void f(T... ts);		// { dg-message "deduction" }

struct B { };
int main()
{
  f<int>(B(), 1);		// { dg-error "" }
}
