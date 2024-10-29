// test that contracts on overriding functions are found correctly
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on -fcontracts-nonattr " }

int foo(const int i) pre( i > 3) post (r: r > 4){

  contract_assert( i > 5);
  return i;
};

int main(int, char**)
{

  foo (1);
}
// { dg-output "contract violation in function foo at .*5: i > 3.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: pre, semantic: observe, mode: predicate_false, terminating: no.(\n|\r\n|\r)" }

// { dg-output "contract violation in function foo at .*7: i > 5.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: assert, semantic: observe, mode: predicate_false, terminating: no.(\n|\r\n|\r)" }

// { dg-output "contract violation in function foo at .*5: r > 4.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: post, semantic: observe, mode: predicate_false, terminating: no.(\n|\r\n|\r)" }
