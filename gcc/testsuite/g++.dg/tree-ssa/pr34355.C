// { dg-do compile }
// { dg-require-effective-target pthread } 
// { dg-options "-O3 -ftree-parallelize-loops=4" }

typedef double EXPRESS[5];

extern int Terms;

void Parse_Rel_Factor (EXPRESS Express, int *Terms)
{
  EXPRESS Local_Express = {5.0, 4.0, 3.0, 2.0, 1.0};
  int Local_Terms = 5;

  int i;

  for (i = (*Terms); i < Local_Terms; i++)
    Express[i] = 0.0;

  Express[i] += Local_Express[i];
}

double Parse_Float ()
{
  EXPRESS Express = {1.0, 2.0, 3.0, 4.0, 5.0};

  Parse_Rel_Factor (Express, &Terms);
}
