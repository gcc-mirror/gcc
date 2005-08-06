/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fdump-tree-vrp" } */

extern void link_error ();

void test01(unsigned int a, unsigned int b)
{
  unsigned int x = 0x80000000;
  if (a < x)
    if (b < x)
      if (a > 5)
        if (a + b == 0U)
          link_error ();
}

void test02(unsigned int a, unsigned int b)
{
  unsigned int x = 0x80000000;
  if (a > x)
    if (b < x)
      if (a - b == 1U)
        link_error ();
}

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
