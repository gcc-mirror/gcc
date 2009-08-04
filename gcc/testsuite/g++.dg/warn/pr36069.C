// PR c++/36069 Strange "warning: suggest parentheses around
// assignment used as truth value" with volatile/non volatile bools
// { dg-do compile }
// { dg-options "-Wparentheses" }
struct foo {
  bool a;
  volatile bool b,c;  
  foo() { a = b = c = false; } // { dg-bogus "parentheses" }
};

int main() {
  bool a;
  volatile bool b,c;
  a = b = c = false; // { dg-bogus "parentheses" }
  foo A;
}
