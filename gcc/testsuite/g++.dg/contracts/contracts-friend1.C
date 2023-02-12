// ensure contracts on friend declarations are a complete class context
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

struct X {
  friend void fn0(X x) [[ pre: x.a > 0 ]] { }

  friend void fn2(X x);
  static void fns0(X x) [[ pre: x.a > 0 ]] { }
  static void fns1(X x) [[ pre: x.a > 0 ]];
  static void fns2(X x);

  friend void fn(X &x) { x.a = -5; }

private:
  int a{10};
};

void fn2(X x) [[ pre: x.a > 0 ]] { }
void X::fns1(X x) { }
void X::fns2(X x) [[ pre: x.a > 0 ]] { }

int main(int, char**) {
  X x;
  fn(x); // no contract

  fn0(x);
  fn2(x);

  X::fns0(x);
  X::fns1(x);
  X::fns2(x);
  return 0;
}

// { dg-output "contract violation in function fn0 at .*.C:6: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function fn2 at .*.C:19: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function X::fns0 at .*.C:9: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function X::fns1 at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function X::fns2 at .*.C:21: .*(\n|\r\n|\r)" }
