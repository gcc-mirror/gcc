/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

extern void link_error ();

void
f3 (unsigned int s)
{
  if ((s & 0x3cc0) == 0)
    {
      if (s >= -15552U)
	link_error ();
    }
  else
    {
      if (s <= 0x3f)
	link_error ();
    }
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
