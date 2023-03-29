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

// { dg-output "contract violation in function main at .*.C:8: x < 10 && y > 123.*(\n|\r\n|\r)" }
