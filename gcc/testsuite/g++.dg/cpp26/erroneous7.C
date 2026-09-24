// PR c++/126848
// { dg-do run { target c++26 } }
// { dg-skip-if "" { *-*-* } { "-ftrivial-auto-var-init=*" } { "" } }
// { dg-options "-O2 -Wuninitialized" }

[[gnu::noipa]]
int
foo (int x)
{
  int a;
  int b = x ? a : 1;
  return b - a;		// { dg-warning "'a' is used uninitialized" }
}

int
main ()
{
  if (foo (1) != 0)
    __builtin_abort ();
}
