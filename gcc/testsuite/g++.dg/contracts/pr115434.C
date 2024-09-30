// We were failing to apply post conditions to void functions.

// { dg-do run }
// { dg-options "-std=c++20 -fcontracts -fcontract-continuation-mode=on" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }


void foo (const int b)
[[ post: b == 9 ]]  // contract not checked
{}

int main()
{
   foo(3);
}

// { dg-output "contract violation in function foo at .*.C:9: b == 9.*(\n|\r\n|\r)" }
