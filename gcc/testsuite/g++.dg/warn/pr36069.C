// PR c++/36069 Strange "warning: suggest parentheses around
// assignment used as truth value" with volatile/non volatile bools
// { dg-do compile }
// { dg-options "-Wparentheses" }
struct foo {
  bool a;
  volatile bool b,c;  
  foo() { a = b = c = false; } // { dg-bogus "parentheses" }
  // { dg-warning "deprecated" "" { target c++2a } .-1 }
};

int main() {
  bool a;
  volatile bool b,c;
  a = b = c = false; // { dg-bogus "parentheses" }
  // { dg-warning "deprecated" "" { target c++2a } .-1 }
  foo A;
}
