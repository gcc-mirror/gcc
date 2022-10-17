// { dg-do compile }
// { dg-options "-O2 -fgimple -fdump-statistics" }
//
// This is a collection of seemingly threadble paths that should not be allowed.

void foobar (int);

// Possible thread from 2->4->3, but it would rotate the loop.
void __GIMPLE (ssa)
f1 ()
{
  int i;

  // Pre-header.
  __BB(2):
  goto __BB4;

  // Latch.
  __BB(3):
  foobar (i_1);
  i_5 = i_1 + 1;
  goto __BB4;

  __BB(4,loop_header(1)):
  i_1 = __PHI (__BB2: 0, __BB3: i_5);
  if (i_1 != 101)
    goto __BB3;
  else
    goto __BB5;

  __BB(5):
  return;

}

// Possible thread from 2->3->5 but threading through the empty latch
// would create a non-empty latch.
void __GIMPLE (ssa)
f2 ()
{
  int i;

  // Pre-header.
  __BB(2):
  goto __BB3;

  __BB(3,loop_header(1)):
  i_8 = __PHI (__BB5: i_5, __BB2: 0);
  foobar (i_8);
  i_5 = i_8 + 1;
  if (i_5 != 256)
    goto __BB5;
  else
    goto __BB4;

  // Latch.
  __BB(5):
  goto __BB3;

  __BB(4):
  return;

}

// Possible thread from 3->5->6->3 but this would thread through the
// header but not exit the loop.
int __GIMPLE (ssa)
f3 (int a)
{
  int i;

  __BB(2):
  goto __BB6;

  __BB(3):
  if (i_1 != 0)
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  foobar (5);
  goto __BB5;

  // Latch.
  __BB(5):
  i_7 = i_1 + 1;
  goto __BB6;

  __BB(6,loop_header(1)):
  i_1 = __PHI (__BB2: 1, __BB5: i_7);
  if (i_1 <= 99)
    goto __BB3;
  else
    goto __BB7;

  __BB(7):
  return;

}

// { dg-final { scan-tree-dump-not "Jumps threaded" "statistics" } }
