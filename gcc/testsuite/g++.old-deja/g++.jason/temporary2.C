class X // Indentation has been done so to see the similarities.
{
public:
  X() {}		  // ERROR - referenced below
         X(X& x) {x.i=7;} // ERROR - Both functions modify the
  void bar(X& x) {x.i=7;} // ERROR - reference parameter x.
  int i;
};

X foo() { X x; return x; }

int main() 
{
  X   x(foo()); // ERROR - Compiler doesn't warn about temporary reference.
  x.bar(foo()); // ERROR - The same mistake is warned about in this case.
}
