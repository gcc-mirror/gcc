// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe" }

int f(int i, int j = 1) pre(i > 0) post(r: r < 5)
{
  contract_assert( j > 0);
  return i;
}


int main(int, char**)
{
  f(5);
  f(0,0);
}
// { dg-output "contract violation in function f at .*: r < 5.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: post, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*: i > 0.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: pre, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*: j > 0.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: assert, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
