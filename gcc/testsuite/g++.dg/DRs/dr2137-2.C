// DR 2137
// { dg-do link { target c++11 } }

// Test that copying Q is better than converting to R.

struct Q {
  Q() { }
  Q(const Q&) { }
};

struct R {
  R(const Q&);
};

void f(Q) { }
void f(R);

int main()
{
  f({Q()});
}
