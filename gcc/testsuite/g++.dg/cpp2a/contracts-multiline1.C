// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

int main(int, char **)
{
  int x = 5;
  int y = 10;
  [[ assert:
    x
    <
    10
    &&
    y
    >
    123
  ]];
}

// { dg-output "default std::handle_contract_violation called: .*.C 8 main x.*10.*y.*123.*(\n|\r\n|\r)*" }
