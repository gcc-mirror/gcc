/* PR lto/125257 */
/* { dg-lto-do link } */
/* { dg-lto-options { "-O0 -flto --param=lto-min-partition=0 --param=lto-max-partition=0" } } */

int main() {}
__attribute__((used)) void foo() {}
