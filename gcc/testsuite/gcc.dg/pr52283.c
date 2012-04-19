/* Test for case labels not integer constant expressions but folding
   to integer constants (used in Linux kernel).  */
/* { dg-do compile } */

extern unsigned int u;

void
b (int c)
{
  switch (c)
    {
    case (int) (2  | ((4 < 8) ? 8 : u)):
      ;
    }
}

