// { dg-skip-if "not yet" { c++ } }

void f (void)
{
  int i, j;
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
  for(i = 1; i < 30; i++)
    {
      if (i == 7) goto out; // { dg-error "invalid branch to/from OpenACC structured block" }
#pragma acc loop
      for(j = 5; j < 10; j++)
	{
	  if (i == 6 && j == 7) goto out; // { dg-error "invalid branch to/from OpenACC structured block" }
	}
    }
 out:
  ;
}
