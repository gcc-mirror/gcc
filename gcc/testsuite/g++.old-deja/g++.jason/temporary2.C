// { dg-do assemble  }
class X // Indentation has been done so to see the similarities.
{
public:
  X() {}		  // { dg-error "" } referenced below
         X(X& x) {x.i=7;} // { dg-error "" } Both functions modify the
  void bar(X& x) {x.i=7;} // { dg-error "" } reference parameter x.
  int i;
};

X foo() { X x; return x; }

int main() 
{
  X   x(foo()); // { dg-error "" } Compiler doesn't warn about temporary reference.
  x.bar(foo()); // { dg-error "" } The same mistake is warned about in this case.
}
