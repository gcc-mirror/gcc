/* Test evolved from source from Simona Perri <perri@mat.unical.it>
   and Gerald Pfeifer<pfeifer@dbai.tuwien.ac.at>.

   Copyright (C) 2003 Free Software Foundation  */

/* { dg-do run } */

#include <stdlib.h>

int main ()
{
  div_t d = div (20, 5);
  if ((d.quot != 4) || (d.rem))
    abort ();
  exit (0);
}
