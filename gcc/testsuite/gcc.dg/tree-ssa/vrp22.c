/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fdump-tree-vrp1" } */
/* { dg-require-effective-target int32plus } */

extern void link_error ();

void test02(unsigned int a, unsigned int b)
{
  unsigned int x = 0x80000000;
  if (a > x)
    if (b < x)
      if (a == b)
        link_error ();
}

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
