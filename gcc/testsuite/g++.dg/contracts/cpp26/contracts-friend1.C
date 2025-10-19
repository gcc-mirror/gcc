// ensure contracts on friend declarations are a complete class context
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe" }


struct X {
  friend void fn0(X x) pre (x.a > 0) { }

  static void fns0(X x) pre ( x.a > 0 ) { }
  static void fns1(X x) pre ( x.a > 0 );
  static void fns2(X x) pre ( x.a > 0 );

  friend void fn(X &x) { x.a = -5; }

private:
  int a{10};
};
void X::fns1(X x) { }
void X::fns2(X x) pre ( x.a > 0 ) { }

int main(int, char**) {
  X x;
  fn(x); // no contract

  fn0(x);

  X::fns0(x);
  X::fns1(x);
  X::fns2(x);
  return 0;
}

// { dg-output "contract violation in function void fn0.X. at .*.C:7: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function static void X::fns0.X. at .*.C:9: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function static void X::fns1.X. at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function static void X::fns2.X. at .*.C:19: .*(\n|\r\n|\r)" }
