/* PR middle-end/60469 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void foo() {}

#define ALEN 1024

int main(int argc, char* argv[])
{
  int b[ALEN];
  b[:] = 100;
  _Cilk_spawn foo();
  return 0;
}
