/* PR middle-end/60469 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus -fdump-tree-original" } */

void foo() {}

#define ALEN 1024

int main(int argc, char* argv[])
{
  int b[ALEN];
  b[:] = 100;
  _Cilk_spawn foo();
  return 0;
}

/* The C++ FE once emitted a bogus error_mark_node for this test case.  */
/* { dg-final { scan-tree-dump-not "<<< error >>>" "original" } } */
