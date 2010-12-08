// { dg-do assemble  }
class X // Indentation has been done so to see the similarities.
{
public:
  X() {}		  // { dg-message "note" } referenced below
         X(X& x) {x.i=7;} // { dg-message "note" } Both functions modify the
  void bar(X& x) {x.i=7;} // { dg-message "note" } reference parameter x.
  int i;
};

X foo() { X x; return x; }

int main() 
{
  X   x(foo()); // { dg-error "no match" } Compiler doesn't warn about temporary reference.
  // { dg-message "candidate" "candidate note" { target *-*-* } 15 }
  x.bar(foo()); // { dg-error "no match" } The same mistake is warned about in this case.
  // { dg-message "candidate" "candidate note" { target *-*-* } 17 }
}
