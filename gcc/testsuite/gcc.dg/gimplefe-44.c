/* { dg-do compile } */
/* { dg-require-effective-target exceptions } */
/* { dg-options "-fexceptions -fgimple -fdump-tree-eh-eh" } */

void __GIMPLE foo()
{
  try
    {
      try
	{
	  extern void might_throw1 ();
	  might_throw1 ();
	}
      finally
	{
	  extern void might_throw2 ();
	  might_throw2 ();
	}
      else
	{
	  extern void might_throw3 ();
	  might_throw3 ();
	}
    }
  finally
    {
      extern void might_throw4 ();
      might_throw4 ();
    }
}

/* { dg-final { scan-tree-dump ".LP 1. might_throw1" "eh" } } */
/* { dg-final { scan-tree-dump ".LP 2. might_throw2" "eh" } } */
/* { dg-final { scan-tree-dump ".LP 2. might_throw3" "eh" } } */
