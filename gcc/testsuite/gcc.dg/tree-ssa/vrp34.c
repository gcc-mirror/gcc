/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void bar (void);

void
foo (int a)
{
  switch (a)
    {
    case 4:
      if (a >= 3)
        if (a <= 5)
          bar ();
    }
}

/* Both ifs should be optimized.  */
/* { dg-final { scan-tree-dump-times "if \\\(" 0 "vrp1" } } */
