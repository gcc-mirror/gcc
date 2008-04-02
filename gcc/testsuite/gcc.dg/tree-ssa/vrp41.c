/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void bar0 (void);
void bar1 (void);
void bar2 (void);
void bar3 (void);

void
foo (int a)
{
  if (a < 100)
    return;
  if (200 < a)
    return;

  switch (a)
    {
    case  99: bar0 (); return;
    case 100: bar1 (); return;
    case 101: bar2 (); return;
    case 102: bar3 (); return;
    }
}

/* { dg-final { scan-tree-dump-not "case 99:" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
