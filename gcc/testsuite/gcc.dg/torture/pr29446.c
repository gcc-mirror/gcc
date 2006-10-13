/* { dg-do compile } */

void f(_Bool D917, int j0, int ubound1, int ubound5)
{
  int i, j = j0;
  int (*abc)[3];
  i = 1;
  while (1)
    {
       if (j <= 3)
         while (1)
           {
              if (i != j)
                {
                  if (ubound1 <= 0)
                    return;
                  (*abc)[1] = 0;
                }
               else
                 {
                    if (j > ubound1)
                      return;
                    if (ubound5 <= 0)
                      return;
                  }
               j = j + 1;
               if (D917)
                 break;
           }
    i = i + 1;
  }
}
