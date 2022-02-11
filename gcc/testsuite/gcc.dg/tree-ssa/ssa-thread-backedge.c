// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-ethread -fdisable-tree-thread1 -fdisable-tree-thread2 -fno-tree-dominator-opts -fdump-tree-threadfull2-details" }

// Test that we can thread jumps across the backedge of a loop through
// the switch statement to a particular case.
//
// Just in case, we disable all the jump threaders before loop
// optimizations to make sure we get a clean stab at this.

int foo (unsigned int x, int s)
{
  while (s != 999)
    {
      switch (s)
	{
	case 0:
	  if (x)
	    s = 1;
	  break;
	case 1:
	  if (x)
	    s = 999;
	  break;
	default:
	  break;
	}
      x++;
    }
  return s;
}

// { dg-final { scan-tree-dump "Registering jump thread:.*normal \\(back\\)" "threadfull2" } }
