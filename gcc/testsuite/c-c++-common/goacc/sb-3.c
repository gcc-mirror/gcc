// { dg-skip-if "not yet" { c++ } }

void f (void)
{
  int i, j;
#pragma acc loop
  for(i = 1; i < 30; i++)
    {
      if (i == 7) goto out; // { dg-error "invalid branch to/from OpenACC structured block" }
#pragma acc loop // { dg-error "work-sharing region may not be closely nested inside of work-sharing, critical, ordered, master or explicit task region" }
      for(j = 5; j < 10; j++)
	{
	  if (i == 6 && j == 7) goto out; // { dg-error "invalid branch to/from OpenACC structured block" }
	}
    }
 out:
  ;
}
