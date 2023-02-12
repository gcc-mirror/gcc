/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread2-stats -fdump-tree-optimized" } */

long a;
int b;
void bar64_(void);
void foo();
int main() {
  char c = 0;
  unsigned d = 10;
  int e = 2;
  for (; d; d--) {
    bar64_();
    b = d;
    e && (c = (e = 0) != 4) > 1;
  }
  if (c < 1)
    foo();
  a = b;
}

/* We need to perform a non-multi-way branch FSM thread creating an
   irreducible loop in thread2 to allow followup threading to
   remove the call to foo ().  */
/* { dg-final { scan-tree-dump "Jumps threaded: 1" "thread2" } } */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
