/* This exposed a bug in tree-ssa-ccp.c.  Since 'j' and 'i' are never
   defined, CCP was not traversing the edges out of the if(), which caused
   the PHI node for 'k' at the top of the while to only be visited once.
   This ended up causing CCP to think that 'k' was the constant '1'.  */
main()
{
  int i, j, k;

  k = 0;
  while (k < 10)
    {
      k++;
      if (j > i)
	j = 5;
      else
	j =3;
    }

  if (k != 10)
    abort ();

  return 0;
}
