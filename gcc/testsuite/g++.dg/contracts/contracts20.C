// test that contract attributes cause errors pre-c++2a
// { dg-do compile { target c++17_only } }

int fun(int a)
  [[ pre: a > 0 ]] // { dg-error "contracts are only available with .-fcontracts." }
  [[ post r: r < 0 ]] // { dg-error "contracts are only available with .-fcontracts." }
{
  [[ assert: a != 0 ]]; // { dg-error "contracts are only available with .-fcontracts." }
  return -a;
}

