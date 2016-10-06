// { dg-do assemble  }
class X // Indentation has been done so to see the similarities.
{
public:
  X() {}
         X(X& x) {x.i=7;} // { dg-message "note" "" { target c++14_down } } Both functions modify the
  void bar(X& x) {x.i=7;} // { dg-message "note" } reference parameter x.
  int i;
};

X foo() { X x; return x; }

int main() 
{
  X   x(foo()); // { dg-error "rvalue" "" { target c++14_down } } Compiler doesn't warn about temporary reference.
  x.bar(foo()); // { dg-error "rvalue" } The same mistake is warned about in this case.
}
