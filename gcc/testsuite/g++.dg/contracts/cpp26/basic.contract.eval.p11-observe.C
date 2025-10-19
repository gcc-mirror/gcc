// N5008 :
// dcl.contract.res/p1
// If a contract violation occurs in a context that is not manifestly constant-evaluated and the evaluation
// semantic is enforce or observe, the contract-violation handler (6.10.3) is invoked with an lvalue referring to
//  n object v of type const std::contracts::contract_violation (17.10.3) containing information about
// the contract violation. Storage for v is allocated in an unspecified manner except as noted in 6.7.6.5.2. The
// lifetime of v persists for the duration of the invocation of the contract-violation handler.
//
// Check the contract violation information in observe semantic
//
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe" }

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
// { dg-output "contract violation in function int f.int, int. at .*: r < 5.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: post, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int f.int, int. at .*: i > 0.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: pre, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int f.int, int. at .*: j > 0.*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: assert, semantic: observe, mode: predicate_false, terminating: no.*(\n|\r\n|\r)" }
