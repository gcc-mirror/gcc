/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-rvrp-details" } */

extern void abort ();
extern void drink ();

void beer (int x)
{
  if (x >= 12 && x <= 15)
    {
      // must be true
      if (x & 0x4)
	{
	  drink ();
	}
      else
        abort();
     return;
    }
  abort();
}

int main()
{
  beer (3);
}

/* { dg-final { scan-tree-dump "Branch rewritten" "rvrp" } } */
