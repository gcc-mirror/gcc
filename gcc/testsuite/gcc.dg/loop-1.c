/* Copyright (C) 2000  Free Software Foundation.

   Simplified from gcc/fold-const.c
   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do compile } */

void
mul_double ()
{
  int i, j, *prod;

  for (i = 0; i < 4; i++)
    {
      for (j = 0; j < 4; j++)
        {
          *prod = 0;
        }
    }
}
