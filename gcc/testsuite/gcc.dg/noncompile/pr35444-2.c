/* PR 35444: ICE from pending VLA sizes in invalid parameter list.
   Similar case to the PR, but with "..." before the syntax error.  */
void foo(int n, int a[n], ... 0); /* { dg-error "expected" } */
void bar() {}
