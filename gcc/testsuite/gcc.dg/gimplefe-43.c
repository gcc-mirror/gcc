/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE foo()
{
  try
    {
      try
	{
	  ;
	}
      finally
	{
	  ;
	}
      else
	{
	  ;
	}
    }
  finally
    {
      ;
    }
}
