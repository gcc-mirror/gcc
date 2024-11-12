// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-continuation-mode=on" }

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

// { dg-output "contract violation in function f at .*: check.15..*(\n|\r\n|\r)" }
// { dg-output ".assertion_kind: pre, semantic: observe, mode: evaluation_exception: threw an instance of .int., terminating: no.*(\n|\r\n|\r)" }
