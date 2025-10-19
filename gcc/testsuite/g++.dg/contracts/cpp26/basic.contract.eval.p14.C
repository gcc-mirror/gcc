// N5008 :
// basic.contract.eval/p14
// Note 10 : If the contract-violation handler returns normally and the evaluation semantic is observe, control flow
// continues normally after the point of evaluation of the contract assertion. — end note
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe" }

bool check(int i){
  if (i > 10)
    throw 3;

  return true;
}

void f() pre(check(15)){}


int main(int, char**)
{

  f();
}

// { dg-output "contract violation in function void f.. at .*: check.15..*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: pre, semantic: observe, mode: evaluation_exception: threw an instance of .int., terminating: no.*(\n|\r\n|\r)" }
