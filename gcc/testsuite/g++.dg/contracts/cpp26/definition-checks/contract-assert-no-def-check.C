// Check that contract asserts are checkef when the definition side contracts
// are turned off
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-definition-check=off -fcontract-evaluation-semantic=observe" }

#include <cstdlib>

bool termintating_check(){
  std::exit(-1);
  return true;
}
// pre and post check would cause termination
void foo(int i) noexcept pre(termintating_check()) post(termintating_check()) {

    contract_assert(i > 4);

}

int main(int, char**)
{

  foo(1);
  return 0;
}
// { dg-output "contract violation in function foo at .*: i > 4.*(\n|\r\n|\r)" }
