/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fdump-tree-vrp-alias" } */

signed char arr[240];
void foo (void)
{

  unsigned short i, length = 200;

  for (i = 1; (int)i < (length - 1); i++)
    arr[i] = -1;
}

/* { dg-final { scan-tree-dump-not "RANGE \\\[0, 65535\\\]" "vrp1" } } */
