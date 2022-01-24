// { dg-do compile }
// { dg-options "-O2 -fgimple -fdump-statistics" }

// This is a collection of threadable paths.  To simplify maintenance,
// there should only be one threadable path per function.

int global;

// The thread from 3->4->5 crosses loops but is allowed because it
// never crosses the latch (BB3) and is just an early exit out of the
// loop.
int __GIMPLE (ssa)
foo1 (int x)
{
  int D_1420;
  int a;

  __BB(2):
  a_4 = ~x_3(D);
  goto __BB4;

  // Latch.
  __BB(3):
  global = a_1;
  goto __BB4;

  __BB(4,loop_header(1)):
  a_1 = __PHI (__BB2: a_4, __BB3: 0);
  if (a_1 != 0)
    goto __BB3;
  else
    goto __BB5;

  __BB(5):
  return;

}

// { dg-final { scan-tree-dump "Jumps threaded\" \"foo1\" 1" "statistics" } }
