void bar (void);
                                                                                
void
foo (int *diff)
{
  double deltay = 0.0;
  int Stangent = 0;
  int mindiff;
  int Sflipped = 0;
  int i;
  int Sturn, Snofit;
                                                                                
  Sturn = 1;
  if (Sturn)
    Stangent = 1;
  if (Sturn)
    {
      Sflipped = 0;
      Snofit = 1;
      while (Snofit)
        {
          Snofit = 0;
          mindiff = 0;
          for (i = 0; i < 4; i++)
            mindiff = diff[i];
          while (!Snofit && (mindiff < 0.0))
            {
              deltay = (Stangent ? deltay : 0.0);
              if (deltay < 0.0)
                Snofit = 1;
              for (i = 0; i < 4; i++)
                {
                }
            }
          if (Snofit)
            if (Sflipped)
              break;
        }
      if (Snofit)
        bar ();
    }
}
