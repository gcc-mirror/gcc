/* { dg-additional-options "-std=gnu89" } */

f ()
{
  if (g ())
    h ();
  else
    {
      do
	{
	  return 0;
	  break;
	}
      while (1);
    }
  return 1;
}
