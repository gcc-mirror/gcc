// PRMS Id: 5368
// Bug: the X temporary in foo() is not destroyed.

int c = 0;

struct X {
  X (int) { c++; }
  ~X() { c--; }
};

struct Y {
   Y(const X &) { }
};

Y foo() { 
  return X(3); 
};

int main()
{
  foo();
  return c;
}
