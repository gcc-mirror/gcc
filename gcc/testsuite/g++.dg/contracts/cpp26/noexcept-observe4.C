//Test that noexcept-observe with a non throwing violation handler has
// observe semantics
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=noexcept_observe " }

int f(const int a, const int b) pre (a > 2) post(r : r > 2){ return 1;};

int main(int, char**)
{
  f(1,1);
  return 0;
}

// { dg-output "contract violation in function f at .*: a > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*: r > 2.*(\n|\r\n|\r)" }
