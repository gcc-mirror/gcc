// test that free functions can be redeclared with contracts without affecting
// normal default parm handling
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

int f(int a, int, int c = 10);
int f(int a, int b = 11, int);
int f(int a, int b, int c)
  [[ pre: a < 0 ]]
  [[ pre: b < 0 ]]
  [[ pre: c < 0 ]];

int f(int, int, int);

int f(int a, int b, int c)
{
  printf("f: a: %d, b: %d, c: %d\n", a, b, c);
  return a * b - c;
}

int f(int a = 12, int, int);

int main(int, char **)
{
  f(1,1,1);
  printf("=====\n");
  f(1,1);
  printf("=====\n");
  f(1);
  printf("=====\n");
  f();
  printf("=====\n");
}

// { dg-output "contract violation in function f at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:11: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "f: a: 1, b: 1, c: 1(\n|\r\n|\r)" }
// { dg-output "=====(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:11: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "f: a: 1, b: 1, c: 10(\n|\r\n|\r)" }
// { dg-output "=====(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:11: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "f: a: 1, b: 11, c: 10(\n|\r\n|\r)" }
// { dg-output "=====(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:11: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "f: a: 12, b: 11, c: 10(\n|\r\n|\r)" }
// { dg-output "=====(\n|\r\n|\r)" }

