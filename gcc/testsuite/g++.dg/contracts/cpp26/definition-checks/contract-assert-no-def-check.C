// Check that contract asserts are checked when the definition side contracts
// are turned off
// { dg-do run { target c++26 } }
// { dg-options "-fcontracts -fcontracts-definition-check=off -fcontract-evaluation-semantic=observe" }

#include <cstdlib>

bool terminating_check(){
  std::exit(-1);
  return true;
}
// pre and post check would cause termination
void foo(int i) noexcept pre(terminating_check()) post(terminating_check()) {

    contract_assert(i > 4);

}

int main(int, char**)
{

  foo(1);
  return 0;
}
// { dg-output "contract violation in function void foo.int. at .*: i > 4.*(\n|\r\n|\r)" }
