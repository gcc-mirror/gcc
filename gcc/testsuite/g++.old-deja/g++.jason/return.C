// PRMS Id: 5331
// Bug: the return value of foo is constructed in a temporary and then
// copied into the return slot.  This is not necessary.

int c = 0;

struct X {
   X(int i) { }
   X(X const &XX) { c = 1; }
   ~X() { }
};

const X foo() { 
  return X(3); 
};

int main()
{
  foo();
  return c;
}
