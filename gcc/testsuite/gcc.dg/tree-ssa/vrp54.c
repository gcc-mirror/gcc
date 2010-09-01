/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

extern void link_error (void);
void foo (void)
{
  int j = 256;
  do
    {
      if (j < 0 || j > 256)
	link_error ();
      j--;
    }
  while (j >= 0);
  if (j != -1)
    link_error ();
}
extern void link_error (void);
void bar (void)
{
  int j = 0;
  do
    {
      if (j < 0 || j > 256)
	link_error ();
      j++;
    }
  while (j <= 256);
  if (j != 257)
    link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
