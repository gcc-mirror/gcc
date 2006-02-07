/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

extern void abort ();
int tree_code_length[100];

blah (int code1)
{
  unsigned char D18670;

  if (code1 != 53) goto L0; else goto L1;

L0:
  abort ();

L1:
  D18670 = tree_code_length[53];
  if (D18670 <= 1) goto L2; else goto L3;

L2:
  abort ();

L3:
  if (D18670 == 2) goto L4; else goto L5;

L4:
  abort ();

L5:
  arf ();
  if (code1 != 53) goto L6; else goto L7;

L6:
  abort ();

L7:
  if (D18670 <= 2) goto L8; else goto L9;

L8:
  abort ();

L9:
  return;

}

/* The second test of (code1 != 53) and the test (D18670 <= 2) are
   both totally subsumed by earlier tests and thus should be folded
   away using VRP.  */
/* { dg-final { scan-tree-dump-times "Folding predicate" 2 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

