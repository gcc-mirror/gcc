/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -maccumulate-outgoing-args -mincoming-stack-boundary=2 -mpreferred-stack-boundary=3 -mregparm=3 -mtune-ctrl=epilogue_using_move" } */

extern void bar (long long int, int);

long long int
fn1 (long long int x)
{
  bar (x, 1);
  return x;
}

/* { dg-final { scan-assembler-not "movl\[ \\t\]+\[0-9]*\\(%esp\\)" } } */
