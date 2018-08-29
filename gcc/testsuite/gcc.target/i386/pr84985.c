/* { dg-do compile } */
/* { dg-options "-O0" } */
int main() {
  int a;
  asm("" : "=d"(a) : "0"(a), "0ae"(&a));
}
